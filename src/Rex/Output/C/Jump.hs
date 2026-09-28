module Rex.Output.C.Jump (
    moduleToCSource
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

commonTemplate :: String
commonTemplate = unsafePerformIO $ do
    dir <- getDataDir
    readFile $ dir ++ "/templates/C/Common.c"
{-# NOINLINE commonTemplate #-}

putStartCodes :: RexModule (DFA enc) -> Output ()
putStartCodes scr = do
    forM_ (scrScMap scr) $ \(name,id) -> 
        when (name /= "0") $ 
            putsLn $ printf "#define %s %d" name id
    putc '\n'

put_rexScan :: A.Impl enc => RexModule (DFA enc) -> Output ()
put_rexScan scr@RexModule { scrAutomata = d } = do
    putsLn "void"
    putsLn "rexScan (int start_code, RexInput const *orig_input)"
    putsLn "{"
    nest 4 $ do
        putsLn "RexAcc accept;"
        putsLn "accept.type = REX_ACC_NONE;"
        putsLn "accept.input = *orig_input;"
        putsLn "int leng = 0;"
    putc '\n'
    -- Put states
    forM_ [0 .. dfaState d - 1] $ \s -> do
        case IM.lookup s (dfaGraph d) of
            Nothing -> putTerminal s
            Just ts -> putNonTerminal s ts
        putc '\n'
    putsLn "}"
  where
    putTerminal s = do
        putsLn $ printf "state_%d:" s
        nest 4 $
            putsLn $ printf "goto accept_%d;" s

    putNonTerminal s ts = do
        putsLn $ printf "state_%d:" s
        nest 4 $ do
            when (s `IM.member` dfaFinal d) $ do
                putsLn $ printf "if (accept.type == REX_ACC_NONE || accept.state == %d) {" s
                nest 4 $
                    putsLn "/* update .accept */"
                putsLn "}"
            putsLn "switch (rexGetChar (&accept.input)) {"
            forM_ (IM.toList ts) $ \(t,cs) -> do
                forM_ (AS.toList cs) $ \c -> 
                    puts $ printf "case %d: " (fromIntegral c :: Int)
                putc '\n'
                nest 4 $ 
                    putsLn $ printf "goto state_%d;" t
            putsLn "default:"
            nest 4 $
                putsLn "goto do_action;"
            putsLn "}"

putScanner :: A.Impl enc => RexModule (DFA enc) -> Output ()
putScanner scr = do
    -- putsLn $ scrPrelude scr
    putsLn commonTemplate
    putStartCodes scr
    put_rexScan scr
    -- put_rexScan scr
    -- putsLn $ scrPostlude scr

moduleToCSource :: A.Impl enc => RexModule (DFA enc) -> String
moduleToCSource = output . putScanner

