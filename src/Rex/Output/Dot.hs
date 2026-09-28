module Rex.Output.Dot (
    moduleToDot
  ) where

import qualified Rex.Data.Alpha as A

import Rex.Module (RexModule(..))
import Rex.Automata.DFA (DFA(..))

import Rex.Output.Monad
import qualified Data.IntMap as IM (toList)
import Control.Monad (forM_)
import Text.Printf (printf)


doDFA :: A.Impl enc => DFA enc -> Output ()
doDFA d = do
    putsLn "digraph DFA {"
    nest 4 $ do
        putsLn "graph ["
        nest 4 $ do
            putsLn "charset = \"UTF-8\""
            putsLn "layout = dot"
            putsLn "rankdir = LR"
        putsLn "];"

        -- Specify the common style of nodes
        putsLn "node ["
        nest 4 $ do
            putsLn "shape = \"circle\""
        putsLn "];"

        -- Specify the style of accepting states
        forM_ (IM.toList (dfaFinal d)) $ \(s,_) -> do
            putsLn $ printf "%d [" s
            nest 4 $ do
                putsLn $ printf "shape = \"doublecircle\";"
            putsLn $ printf "];"

        -- Draw initial state nodes
        forM_ ([0 .. dfaStart d - 1] `zip` [-1, -2 ..]) $ \(s,i) -> do
            putsLn $ printf "%d [shape = point];" (i :: Int)
            putsLn $ printf "%d;" s
            putsLn $ printf "%d -> %d;" i s
        -- Draw other state nodes
        forM_ (IM.toList (dfaGraph d)) $ \(s,ts) -> 
            forM_ (IM.toList ts) $ \(t,c) -> do
            putsLn $ printf "%d;" s
            putsLn $ printf "%d;" t
            putsLn $ printf "%d -> %d [label = %s];" s t (show (show c))

    putsLn "}"

moduleToDot :: A.Impl enc => RexModule (DFA enc) -> String
moduleToDot = output . doDFA . scrAutomata

