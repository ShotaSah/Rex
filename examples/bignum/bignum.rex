{
import Text.Printf (printf)
import UTF8Wrapper
}

$all_chars   = [.\n]
$digit       = [0-9]
$other_chars = $all_chars # $digit

@bignum    = $digit{4,} | $digit{1,3} ("," $digit{3})+
@spaces    = $digit{1,2} | $other_chars+

%%

@bignum    { () }
@spaces    ;

{
seeBignums :: RexInput -> IO ()
seeBignums inp = 
    case rexScan 0 inp of
    RexError (_, txt) -> 
        printf "lexical error: %s\n" (take 10 txt)
    RexEOF -> 
        pure ()
    RexToken () inp0@(_, txt) len inp1 -> do
        printf "Saw %s\n" (take len txt)
        seeBignums inp1

main :: IO ()
main = do
    inp <- getContents
    seeBignums $ fromString inp
}
