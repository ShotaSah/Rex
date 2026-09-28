-- 
-- An implementation of DFA minimizing function.
-- 
module Rex.Automata.DFAMin (
    minimizeDFA
  ) where

import Rex.Automata.DFA
import qualified Rex.Data.Alpha as A
import qualified Rex.Data.AlphaSet as AS

import Data.Map (Map)
import qualified Data.Map as M
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Foldable (foldl')


type EqvClass = IntSet

-- Find quotient set of DFA states induced by Myhill─Nerode relation.
quotient :: A.Impl enc => DFA enc -> [EqvClass]
quotient d = 
    loop init_p init_w
  where
    states = IS.fromList [0 .. dfaState d - 1]
    finals = M.elems
           $ foldl' (\m (k,v) -> M.insertWith IS.union v (IS.singleton k) m)
                    M.empty (IM.toList (dfaFinal d))
    others = states IS.\\ IS.unions finals
    init_p = finals ++ [others | not (IS.null others)]
    init_w = init_p

    invMap c a = 
        IS.fromList [x | s <- IS.toList a, x <- inv c s]
      where
        inv c s = IM.findWithDefault [] s (IM.findWithDefault IM.empty c f)
        f = IM.fromListWith (IM.unionWith (++))
           [(fromIntegral c, IM.fromListWith (++) [(t, [s])])
             | (s,ts) <- IM.toList (dfaGraph d)
             , (t,cs) <- IM.toList ts
             , c <- AS.toList cs]

    loop p [] = p
    loop p (a:w) = loop' p w (AS.toList (dfaAlpha d))
      where
        loop' p w [] = loop p w
        loop' p w (c:cs) = repl_p [] p w
          where
            x = invMap (fromIntegral c) a
            repl_p p [] w = loop' p w cs
            repl_p p (y:ys) w
              | IS.null i || IS.null d = repl_p (y:p) ys w
              | otherwise = repl_p (i:d:p) ys (repl_w w)
              where
                i = x `IS.intersection` y
                d = y IS.\\ x
                repl_w [] = if IS.size i <= IS.size d then [i] else [d]
                repl_w (z:w)
                  | y == z    = i : d : w
                  | otherwise = z : repl_w w

-- Consider a equivalent class as a new DFA state and construct a map 
-- relabeling nodes in original graph into new label.
renaming :: DFA enc -> [EqvClass] -> DFAState -> DFAState
renaming d ks = 
    \s -> f IM.! s
  where
    f = IM.fromList
       [(s, t) | (k, t) <- bijMap, s <- IS.toList k]

    bijMap = 
        go ks (dfaStart d)
      where
        go (k:ks) t = case IS.lookupLT (dfaStart d) k of
            Nothing -> (k, t) : go ks (t + 1)
            Just s  -> (k, s) : go ks t
        go [] _ = []

-- Relabel nodes in graph and make duplicated nodes into one.
compact :: DFA enc -> [EqvClass] -> DFA enc
compact d ks = d {
    dfaState = length ks
  , dfaGraph = renameGraph (dfaGraph d)
  , dfaFinal = renameFinal (dfaFinal d)
  }
  where
    rename = renaming d ks
    renameGraph = IM.mapKeysWith (IM.unionWith AS.union) rename
                . IM.map (IM.mapKeysWith AS.union rename)
    renameFinal = IM.mapKeys rename

-- Transform a given DFA into the equivalent minimum DFA.
minimizeDFA :: A.Impl enc => DFA enc -> DFA enc
minimizeDFA d = compact d (quotient d)

