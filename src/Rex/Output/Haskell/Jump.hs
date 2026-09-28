module Rex.Output.Haskell.Jump (
    moduleToHsSource
  ) where

import Rex.Module (RexModule(..))
import Rex.Automata.DFA (DFA(..))
import qualified Rex.Data.Alpha as A
import qualified Rex.Data.AlphaSet as AS

import Rex.Output.Monad

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Control.Monad (forM_, when)
import Text.Printf (printf)

import System.IO.Unsafe (unsafePerformIO)
import Paths_executable_rex (getDataDir)


template :: String
template = unsafePerformIO $ do
    dir <- getDataDir
    readFile $ dir ++ "/templates/Haskell/Jump.hs"
{-# NOINLINE template #-}

putStartCodes :: RexModule (DFA enc) -> Output ()
putStartCodes scr = 
    forM_ (scrScMap scr) $ \(name,id) -> 
    when (name /= "0") $ do
        putsLn $ printf "%s :: Int" name
        putsLn $ printf "%s = %d" name id
        putc '\n'

putActions :: RexModule (DFA enc) -> Output ()
putActions scr = 
    forM_ (scrActionMap scr) $ \(id,act) -> 
    case act of
    Nothing -> pure ()
    Just code -> do
        putsLn $ printf "rex_action_%d = " id
        nest 4 $ putsLn code
        putc '\n'

put_rexScan :: A.Impl enc => RexModule (DFA enc) -> Output ()
put_rexScan scr@RexModule { scrAutomata = d } = do
    putsLn "-- rexScan :: Int -> RexInput -> RexReturn _"
    putsLn "rexScan sc inp0 = "
    nest 4 $ do
        putsLn "case state_0 RexLccNone 0 inp0 of"
        putsLn "(RexLccNone,inp1) -> case rexGetChar 0 inp1 of"
        nest 4 $ do
            putsLn "Just {} -> RexError inp1"
            putsLn "Nothing -> RexEOF"
        putsLn "(RexLccSkip inp1,_) -> rexScan sc inp1"
        putsLn "(RexLcc act inp0 len inp1,_) -> RexToken act inp0 len inp1"
    nest 2 $ putsLn "where"
    nest 4 $ do
        -- Put states
        forM_ [0 .. dfaState d - 1] $ \s -> do
            case IM.lookup s (dfaGraph d) of
                Nothing -> putTerminal s
                Just ts -> putNonTerminal s ts
            putc '\n'
        -- Put final states
        forM_ (IM.toList (dfaFinal d)) $ \(s,ts) -> do
            putAccept s ts
            putc '\n'
        -- Put actions
        forM_ (scrActionMap scr) $ \(id,code) -> do
            putAction id code
            putc '\n'
  where
    putTerminal s = do
        putsLn $ printf "state_%d acc len0 inp0 = acc `seq` len0 `seq` inp0 `seq`" s
        nest 4 $ 
            putsLn $ printf "(accept_%d len0 inp0, inp0)" s

    putNonTerminal s ts = do
        putsLn $ printf "state_%d acc len0 inp0 = acc `seq` len0 `seq` inp0 `seq`" s
        nest 4 $ 
            putsLn $ if s `IM.member` dfaFinal d
            then printf "shift_%d (accept_%d len0 inp0) len0 inp0" s s
            else printf "shift_%d acc len0 inp0" s
        putc '\n'
        putsLn $ printf "shift_%d acc len0 inp0 = acc `seq` len0 `seq` inp0 `seq`" s
        nest 4 $ do
            putsLn "case rexGetChar len0 inp0 of"
            putsLn "Just (c,len1,inp1) -> (\\next -> next acc len1 inp1) $ case c of"
            nest 4 $ do
                forM_ (IM.toList ts) $ \(t,cs) -> 
                  forM_ (AS.toList cs) $ \c -> 
                    putsLn $ printf "%d -> state_%d" (fromIntegral c :: Int) t
                putsLn "_ -> \\_ _ _ -> (acc,inp0)"
            putsLn "Nothing -> (acc,inp0)"

    putAccept s ts = do
        putsLn $ printf "accept_%d len inp = " s
        nest 4 $ do
            putsLn "case (sc :: Int) of"
            forM_ (IM.toList ts) $ \(sc,aid) -> 
                putsLn $ printf "%d -> action_%d len inp" sc aid
            putsLn "_ -> error \"rexScan: undefined start code\""

    putAction id act = do
        putsLn $ printf "action_%d len inp1 = " id
        nest 4 $ 
            putsLn $ case act of
            Nothing -> "RexLccSkip inp1"
            Just {} -> printf "RexLcc rex_action_%d inp0 len inp1" id

putScanner :: A.Impl enc => RexModule (DFA enc) -> Output ()
putScanner scr = do
    putsLn $ scrPrelude scr
    putsLn template
    putStartCodes scr
    putActions scr
    put_rexScan scr
    putsLn $ scrPostlude scr

moduleToHsSource :: A.Impl enc => RexModule (DFA enc) -> String
moduleToHsSource = output . putScanner

