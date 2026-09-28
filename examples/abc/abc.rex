{
import UTF8Wrapper
}

%%

"abc"    { putStrLn "abc" }
"abcd"   { putStrLn "abcd" }
"abcde"  { putStrLn "abcde" }

{
seeAbcs :: RexInput -> IO ()
seeAbcs inp = 
    case rexScan 0 inp of
    RexError inp1 -> 
        putStrLn "lexical error"
    RexEOF -> 
        pure ()
    RexToken act _inp0 _len inp1 -> do
        act
        seeAbcs inp1

main :: IO ()
main = 
  
  seeAbcs $ fromString "abcdeabcabcdabcabcdeabcabcdabc"
}

