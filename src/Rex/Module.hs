{-# LANGUAGE StrictData #-}
{-# LANGUAGE Rank2Types #-}

module Rex.Module (
    ScName
  , StartCode
  , Code
  , Action
  , ActionId
  , ActionId'
  , PreNFA
  , RexModule(..)
  , makeRexModule
  ) where

import Rex.Syntax
import Rex.Data.CharSet (CharSet)
import qualified Rex.Data.CharSet as CS
import Rex.Data.Regex (Regex(..))
import qualified Rex.Data.Regex as RE

import qualified Data.Set as S
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Foldable (foldl')
import Control.Monad (forM)


data RexModule a = RexModule {
    scrPrelude :: Code
  , scrPostlude :: Code
  , scrAutomata :: a
  , scrScMap :: [(ScName, StartCode)]
  , scrActionMap :: [(ActionId, Action)]
  } deriving Show

instance Functor RexModule where
    fmap t scr@RexModule { scrAutomata = a } = 
        scr { scrAutomata = t a }

type StartCode = Int
type ActionId = Int
type ActionId' = (StartCode, ActionId)
type PreNFA enc = [(Regex enc, ActionId')]

newtype Eval enc a = Eval
  { unEval :: forall r.
             (a -> CsEnv -> ReEnv enc -> r)
          -> (String -> r)
          -> CsEnv -> ReEnv enc
          -> r }

type CsEnv = Map CsName (Either CsExpr CharSet)
type ReEnv enc = Map ReName (Either ReExpr (Regex enc))

eval :: Eval enc r -> CsEnv -> ReEnv enc -> Either String r
eval m = unEval m (\r _ _ -> Right r) Left

instance Functor (Eval enc) where
    fmap f mx = Eval $ \cont -> unEval mx (cont . f)

instance Applicative (Eval enc) where
    mf <*> mx = Eval $ \cont fail -> unEval mf (\f -> unEval mx (cont . f) fail) fail
    pure x = Eval $ \cont _fail -> cont x

instance Monad (Eval enc) where
    mx >>= k = Eval $ \cont fail -> unEval mx (\x -> unEval (k x) cont fail) fail

evalError :: String -> Eval enc a
evalError msg = Eval $ \_cont fail _ _ -> fail msg

askCsEnv :: Eval enc CsEnv
askCsEnv = Eval $ \cont _fail cs -> cont cs cs

askReEnv :: Eval enc (ReEnv enc)
askReEnv = Eval $ \cont _fail cs re -> cont re cs re

updCsEnv :: CsName -> CharSet -> Eval enc ()
updCsEnv k v = v `seq` Eval $ \cont _fail cs -> let
    cs' = M.insert k (Right v) cs
 in cs' `seq` cont () cs'

updReEnv :: ReName -> Regex enc -> Eval enc ()
updReEnv k v = v `seq` Eval $ \cont _fail cs re -> let
    re' = M.insert k (Right v) re
 in re' `seq` cont () cs re'

evalCs :: CsExpr -> Eval enc CharSet
evalCs exp = 
    case exp of
        CsEmpty -> pure CS.empty
        CsNonNL -> pure (CS.every CS.\\ CS.singleton '\n')
        CsOne c -> pure (CS.singleton c)
        CsRange c d -> pure (CS.range c d)
        CsUnion xs -> CS.unions <$> (mapM evalCs xs)
        CsDiff x y -> CS.difference <$> evalCs x <*> evalCs y
        CsVar k -> force k
  where
    force k = do
        env <- askCsEnv
        case M.lookup k env of
            Nothing -> evalError ("unbound variable: " ++ k)
            Just (Right r) -> pure r
            Just (Left th) -> do
                r <- evalCs th
                updCsEnv k r
                pure r

evalRe :: RE.Impl enc => ReExpr -> Eval enc (Regex enc)
evalRe exp = 
    case exp of
        ReEmpty -> pure Empty
        ReEps -> pure Eps
        ReCh ce -> RE.encode <$> evalCs ce
        ReUnion x y -> Union <$> evalRe x <*> evalRe y
        ReCat x y -> Cat <$> evalRe x <*> evalRe y
        ReStar x -> Star <$> evalRe x
        ReVar k -> force k
  where
    force k = do
        env <- askReEnv
        case M.lookup k env of
            Nothing -> evalError ("unbound variable: " ++ k)
            Just (Right r) -> pure r
            Just (Left th) -> do
                r <- evalRe th
                updReEnv k r
                pure r

mkEnvs :: [Decl] -> (CsEnv, ReEnv enc)
mkEnvs = 
    foldl' ins (M.empty, M.empty)
  where ins (cs,re) (CsBind x e) = (M.insert x (Left e) cs, re)
        ins (cs,re) (ReBind x e) = (cs, M.insert x (Left e) re)

mkPreNFA :: RE.Impl enc => [Rule]
         -> Eval enc (PreNFA enc, [(ScName, StartCode)], [(ActionId, Action)])
mkPreNFA rules = do
    prenfa <- forM rules' $ \(sc, (rexp, _), aid) -> do
        re <- evalRe rexp
        pure (re, (scmap M.! sc, aid))
    pure (prenfa, M.toList scmap, actmap)
  where
    allscs = S.toList (S.fromList ("0":[sc | Rule scs _ <- rules, sc <- scs]))
    rules' = [(sc, bind, aid)
               | ((scs, bind), aid) <- 
                   [(scs, bind) | Rule scs binds <- rules, bind <- binds]
                   `zip` [0..]
               , sc <- if null scs then allscs else scs]
    scmap  = M.fromList (allscs `zip` [0 ..])
    actmap = [(aid, act) | (_, (_, act), aid) <- rules']

interpret :: RE.Impl enc => Module -> Eval enc (RexModule (PreNFA enc))
interpret mod = do
    (prenfa, scmap, actmap) <- mkPreNFA rules
    pure RexModule {
        scrPrelude = prelude
      , scrPostlude = postlude
      , scrAutomata = prenfa
      , scrScMap = scmap
      , scrActionMap = actmap
      }
  where
    Module {
        rexPrelude = prelude
      , rexPostlude = postlude
      , rexRules = rules
      } = mod

makeRexModule :: RE.Impl enc => Module -> Either String (RexModule (PreNFA enc))
makeRexModule mod = uncurry (eval (interpret mod)) (mkEnvs (rexDecls mod))

