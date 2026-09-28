{-# LANGUAGE StrictData #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

-- 
-- Generate DFA from NFA
-- 
module Rex.Automata.DFA (
    DFAGraph
  , DFAState
  , DFAFinal
  , DFA(..)
  , nfaToDFA
  ) where

import Rex.Module
import Rex.Automata.NFA
import Rex.Codec.Types (Encoding)
import Rex.Data.Alpha (Alpha)
import qualified Rex.Data.Alpha as A
import Rex.Data.AlphaSet (AlphaSet)
import qualified Rex.Data.AlphaSet as AS

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Control.Monad (forM_)


type DFATrans enc = IntMap (AlphaSet enc)
type DFAGraph enc = IntMap (DFATrans enc)
type DFAState = Int
type DFAFinal = IntMap (IntMap ActionId)

data DFA (enc :: Encoding) = DFA {
    dfaState :: Int           -- set of all states: [0 .. dfaState - 1]
  , dfaAlpha :: AlphaSet enc  -- collection of alphabets in graph
  , dfaGraph :: DFAGraph enc  -- transition function of DFA
  , dfaStart :: DFAState      -- the initial state
  , dfaFinal :: DFAFinal      -- map from accepting states to actions
  } deriving Show

type Env = Map AmbState DFAState
type AmbState = IntSet

newtype GenDFA enc a = Gen
  { unGen :: (a -> DFAState -> Env -> DFAGraph enc -> DFA enc)
          -> DFAState -> Env -> DFAGraph enc
          -> DFA enc }

genDFA :: GenDFA enc () -> NFA enc -> DFA enc
genDFA gen n = 
    unGen gen terminalk 0 M.empty IM.empty
  where terminalk () _ e g = DFA {
            dfaState = M.size e
          , dfaAlpha = nfaAlpha n
          , dfaGraph = g
          , dfaStart = 1
          , dfaFinal = mkFinal e
          }
        mkFinal e = IM.fromListWith (IM.unionWith (\_ y -> y))
           [(s, IM.singleton sc co)
                   | (a, s) <- M.toList e
                   , (t, (sc, co)) <- nfaFinal n
                   , t `IS.member` a]

instance Functor (GenDFA enc) where
    fmap f mx = Gen $ \cont -> unGen mx (cont . f)

instance Applicative (GenDFA enc) where
    mf <*> mx = Gen $ \cont -> unGen mf (\f -> unGen mx (cont . f))
    pure x = Gen ($ x)

instance Monad (GenDFA enc) where
    mx >>= k = Gen $ \cont -> unGen mx (\x -> unGen (k x) cont)

newState :: AmbState -> GenDFA enc DFAState
newState a = 
    Gen $ \cont s e -> let
        s' = s + 1
        e' = M.insert a s e
     in s' `seq` e' `seq` cont s s' e'

askState :: AmbState -> GenDFA enc DFAState
askState a = Gen $ \cont s e -> cont (e M.! a) s e

lookupState :: AmbState -> GenDFA enc (Maybe DFAState)
lookupState a = Gen $ \cont s e -> cont (M.lookup a e) s e

updGraph :: DFAState -> (Maybe (DFATrans enc) -> Maybe (DFATrans enc))
         -> GenDFA enc ()
updGraph x f = Gen $ \cont s e g -> cont () s e $! IM.alter f x g

alphaEdge :: A.Impl enc => Alpha enc -> DFAState -> DFAState -> GenDFA enc ()
alphaEdge c s t = updGraph s $ 
    Just
      . IM.insertWith AS.union t (AS.singleton c)
      . maybe IM.empty id

epsilonClosure :: NFA enc -> AmbState -> AmbState
epsilonClosure n a = 
    IS.unions (a : map (go []) (IS.toList a))
  where
    go occ s | s `elem` occ = IS.empty  -- break cycle
    go occ s = 
        case IM.lookup s (nfaGraph n) of
        Nothing -> IS.empty
        Just NFATrans {transEps = eps} -> 
            IS.unions (IS.fromList eps : map (go (s:occ)) eps)

alphaClosure :: A.Impl enc => NFA enc -> Alpha enc -> AmbState -> AmbState
alphaClosure n c = 
    IS.unions . map go . IS.toList
  where
    go s = 
        case IM.lookup s (nfaGraph n) of
        Nothing -> IS.empty
        Just NFATrans {transChr = chr} -> 
            IS.fromList [t | (t,set) <- chr, c `AS.member` set]

-- Do powerset construction for given NFA
doNFA :: A.Impl enc => NFA enc -> GenDFA enc ()
doNFA n = do
    let a = epsilonClosure n (IS.fromList [0 .. nfaStart n - 1])
    _ <- newState a
    go a
  where
    go a = 
        forM_ (AS.toList (nfaAlpha n)) $ \c -> do
            let b = epsilonClosure n (alphaClosure n c a)
            ret <- lookupState b
            case ret of
              Nothing | not (IS.null b) -> do
                  s <- askState a
                  t <- newState b
                  alphaEdge c s t
                  go b
              Nothing ->
                  pure ()
              Just t -> do
                  s <- askState a
                  alphaEdge c s t

-- Transform a given NFA into a DFA that recognize the same language.
nfaToDFA :: A.Impl enc => NFA enc -> DFA enc
nfaToDFA n = genDFA (doNFA n) n

