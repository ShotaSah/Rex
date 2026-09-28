{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main (
    main
  ) where

import qualified Rex.Data.Alpha as A
import Rex.Data.Alpha.UTF8  ()
import Rex.Data.Alpha.UTF16 ()
import Rex.Data.Alpha.UTF32 ()
import qualified Rex.Data.Regex as RE
import Rex.Data.Regex.UTF8  ()
import Rex.Data.Regex.UTF16 ()
import Rex.Data.Regex.UTF32 ()
import Rex.Codec.Types (Encoding(..))

import Rex.Syntax (parseModule)
import Rex.Module (RexModule(..), makeRexModule)
import Rex.Automata.NFA (rawToNFA)
import Rex.Automata.DFA (nfaToDFA)
import Rex.Automata.DFAMin (minimizeDFA)
import Rex.Output.C (moduleToCSource)
import Rex.Output.Haskell (moduleToHsSource)
import Rex.Output.Dot (moduleToDot)

import System.Environment (getArgs, getProgName)
import System.Exit (exitWith, ExitCode(..))
import System.FilePath
import System.IO 
import Control.Exception (evaluate)
import Control.Monad (when, mplus)
import Data.Maybe (fromJust)
import Data.Proxy (Proxy(Proxy))

import Data.Version (showVersion)
import Paths_executable_rex (version)


quit :: String -> IO a
quit msg = putStrLn msg >> exitWith ExitSuccess

interrupt :: String -> IO a
interrupt msg = do
    name <- getProgName
    putStrLn (name ++ ": " ++ msg) >> exitWith (ExitFailure 1)

data Target = 
    C    -- C source
  | CXX  -- C++ source
  | Hs   -- Haskell source
  | Dot  -- dot file

extToTarget :: String -> Maybe Target
extToTarget "c"   = Just C
extToTarget "cpp" = Just CXX
extToTarget "cxx" = Just CXX
extToTarget "hs"  = Just Hs
extToTarget "dot" = Just Dot
extToTarget _     = Nothing

targetToExt :: Target -> String
targetToExt C   = "c"
targetToExt CXX = "cpp"
targetToExt Hs  = "hs"
targetToExt Dot = "dot"

nameToCodec :: String -> Maybe Encoding
nameToCodec "utf8"  = Just UTF8
nameToCodec "utf16" = Just UTF16
nameToCodec "utf32" = Just UTF32
nameToCodec _       = Nothing

data RexArg = RexArg {
    argTarget :: Maybe Target  -- kind of output
  , argInFile :: FilePath  -- input file name
  , argOutFile :: FilePath  -- output file name
  , argVerb :: Bool  -- verbosity
  , argCodec :: Encoding  -- encoding format of input stream for generated lexer
  }

defaultRexArg :: RexArg
defaultRexArg = RexArg {
    argTarget = Nothing
  , argInFile = ""
  , argOutFile = ""
  , argVerb = False
  , argCodec = UTF8
  }

guessRexArg :: RexArg -> Either String RexArg
guessRexArg arg = do
    when (null (argInFile arg)) $
        Left "input file unspecified"
    let 
        RexArg { argTarget = mtgt, argInFile = ifile, argOutFile = ofile } = arg
        tgt = maybe Dot id mtgt
        ext = targetToExt tgt
        out = if null ofile then
                  case takeExtension ifile of
                  "" -> ifile  <.> ext
                  _  -> ifile -<.> ext
              else ofile
    pure arg { argTarget = Just tgt, argOutFile = out }

data Task = 
    DoRex RexArg  -- generate a lexer from input file
  | Help          -- display help
  | Version       -- display version of this program

getTask :: IO (Either String Task)
getTask = do
    arg <- getArgs
    pure (go arg defaultRexArg)
  where
    do_t arg kind = case extToTarget kind of
        Nothing  -> Left ("unknown output format: " ++ kind)
        Just tgt -> Right arg { argTarget = Just tgt }

    do_i arg file = Right arg { argInFile = file }
    do_o arg file = Right arg { argOutFile = file }

    do_v arg = Right arg { argVerb = True }

    do_c arg kind = case nameToCodec kind of
        Nothing  -> Left ("unknown encoding format: " ++ kind)
        Just enc -> Right arg { argCodec = enc } 

    go [] arg = 
        DoRex <$> guessRexArg arg
    go opts arg = case opts of
        "-o":a1:opts' -> go opts' =<< do_o arg a1 
        ('-':'o':a1):opts' -> go opts' =<< do_o arg a1
        "-T":a1:opts' -> go opts' =<< do_t arg a1
        ('-':'T':a1):opts' -> go opts' =<< do_t arg a1
        "-C":a1:opts' -> go opts' =<< do_c arg a1
        ('-':'C':a1):opts' -> go opts' =<< do_c arg a1
        "-v":opts' -> go opts' =<< do_v arg
        "-V":_ -> Right Version
        "--version":_ -> Right Version
        "-?":_ -> Right Help
        "--help":_ -> Right Help
        ('-':_):_ -> Left "parsing failed during command line options"
        file:opts' -> go opts' =<< do_i arg file

doRex' :: forall enc. (A.Impl enc, RE.Impl enc) => Proxy enc
       -> RexArg -> IO ()
doRex' Proxy arg = do
    raw <- withFile (argInFile arg) ReadMode $ \h -> do
        hSetEncoding h utf8
        inp <- hGetContents h
        scr <- either interrupt pure $ do
            syn <- parseModule inp
            makeRexModule @enc syn
        evaluate scr
    nfa <- generating "NFA" rawToNFA raw
    dfa <- generating "DFA" nfaToDFA nfa
    min <- generating "minimized DFA" minimizeDFA dfa
    withFile (argOutFile arg) WriteMode $ \h -> do
        hSetEncoding h utf8
        hPutStrLn h $ ($ min) $ case fromJust (argTarget arg) of
            C   -> moduleToCSource
            CXX -> moduleToCSource
            Hs  -> moduleToHsSource
            Dot -> moduleToDot
  where
    generating name trans scr = do
        when (argVerb arg) $ do
            putStr $ "Generating " ++ name ++ "..."
            hFlush stdout
        res <- evaluate (fmap trans scr)
        when (argVerb arg) $ do
            putStr $ "done.\n"
            print (scrAutomata res)
        pure res

doRex :: RexArg -> IO ()
doRex arg = 
    case argCodec arg of
        UTF8  -> doRex' @UTF8  Proxy arg
        UTF16 -> doRex' @UTF16 Proxy arg
        UTF32 -> doRex' @UTF32 Proxy arg

helpText :: String
helpText = "help"

versionText :: String
versionText = "Rex " ++ showVersion version

main :: IO ()
main = do
    task <- getTask
    case task of
        Right (DoRex arg) -> doRex arg
        Right Help -> quit helpText
        Right Version -> quit versionText
        Left msg -> interrupt msg 
