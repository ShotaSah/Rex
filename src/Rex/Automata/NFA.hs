{-# LANGUAGE StrictData #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

-- 
-- Generate NFA from raw scanner
-- 
module Rex.Automata.NFA (
    NFATrans(..)
  , NFAGraph
  , NFAState
  , NFAFinal
  , NFA(..)
  , rawToNFA
  ) where

import Rex.Module (RexModule, PreNFA, ActionId')
import Rex.Data.Regex (Regex(..))
import Rex.Data.AlphaSet (AlphaSet)
import qualified Rex.Data.AlphaSet as AS
import Rex.Codec.Types (Encoding)

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Control.Monad (forM_)


data NFATrans enc = NFATrans {
    transEps :: [NFAState]
  , transChr :: [(NFAState, AlphaSet enc)]
  } deriving Show

type NFAGraph enc = IntMap (NFATrans enc)
type NFAState = Int
type NFAFinal = [(NFAState, ActionId')]

data NFA (enc :: Encoding) = NFA {
    nfaState :: Int           -- set of all states: [0 .. nfaState - 1]
  , nfaAlpha :: AlphaSet enc  -- collection of all alphabets in graph
  , nfaGraph :: NFAGraph enc  -- transition function of NFA
  , nfaStart :: Int           -- set of all initial states: [0 .. nfaStart - 1]
  , nfaFinal :: NFAFinal      -- map from accepting states to actions
  } deriving Show

newtype GenNFA enc a = Gen
  { unGen :: (a -> NFAState -> NFAFinal -> NFAGraph enc -> NFA enc)
          -> NFAState -> NFAFinal -> NFAGraph enc
          -> NFA enc }

genNFA :: GenNFA enc () -> NFAState -> NFA enc
genNFA gen s = 
    unGen gen terminalk s [] IM.empty
  where terminalk () s f g = NFA {
            nfaState = s
          , nfaAlpha = mkAlpha g
          , nfaGraph = g
          , nfaStart = 1
          , nfaFinal = f
          }
        mkAlpha g = AS.unions [cs | t <- IM.elems g, (_, cs) <- transChr t]

instance Functor (GenNFA enc) where
    fmap f mx = Gen $ \cont -> unGen mx (cont . f)

instance Applicative (GenNFA enc) where
    mf <*> mx = Gen $ \cont -> unGen mf (\f -> unGen mx (cont . f))
    pure x = Gen ($ x)

instance Monad (GenNFA enc) where
    mx >>= k = Gen $ \cont -> unGen mx (\x -> unGen (k x) cont)

newState :: GenNFA enc NFAState
newState = Gen $ \cont s -> cont s $! s + 1

addFinal :: NFAState -> ActionId' -> GenNFA enc ()
addFinal a z = Gen $ \cont s f -> cont () s ((a, z):f)

updGraph :: NFAState -> (Maybe (NFATrans enc) -> Maybe (NFATrans enc))
         -> GenNFA enc ()
updGraph a upd = Gen $ \cont s f g -> cont () s f $! IM.alter upd a g

epsilonEdge :: NFAState -> NFAState -> GenNFA enc ()
epsilonEdge s t | s /= t = 
    updGraph s $ \x -> Just $ case x of
        Nothing -> NFATrans [t] []
        Just (NFATrans eps chr) -> NFATrans (t:eps) chr
epsilonEdge _ _ = pure ()

alphaEdge :: NFAState -> NFAState -> AlphaSet enc -> GenNFA enc ()
alphaEdge s t set = 
    updGraph s $ \x -> Just $ case x of
        Nothing -> NFATrans [] [(t, set)]
        Just (NFATrans eps chr) -> NFATrans eps ((t, set):chr)

doRegex :: NFAState -> NFAState -> Regex enc -> GenNFA enc ()
doRegex _ _ Empty = 
    pure ()
doRegex s t Eps = 
    epsilonEdge s t
doRegex s t (Ch set) = 
    alphaEdge s t set
doRegex s t (Union x y) = do
    doRegex s t x
    doRegex s t y
doRegex s t (Cat x y) = do
    a <- newState
    doRegex s a x
    doRegex a t y
doRegex s t (Star x) = do
    a <- newState
    epsilonEdge s a
    doRegex a a x
    epsilonEdge a t
{-
doRegex s t (Plus x) = do
    a <- newState
    b <- newState
    epsilonEdge s a
    doRegex a b x
    doRegex b b x
    epsilonEdge b t
doRegex s t (Ques x) = do
    epsilonEdge s t
    doRegex s t x
-}

doPreNFA :: PreNFA enc -> GenNFA enc ()
doPreNFA rules = 
    forM_ rules $ \(re, act) -> do
        f <- newState
        doRegex 0 f re
        addFinal f act

rawToNFA :: PreNFA enc -> NFA enc
rawToNFA t = genNFA (doPreNFA t) (length t)

