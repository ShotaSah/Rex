{
import Control.Monad.Trans
import Control.Monad.Trans.State
import System.IO
import Text.Printf (printf)
import UTF8Wrapper
}

$spaces   = [.\n]

%%

"Alyosha" { modify (+ 1) }
$spaces   ;

{
seeAlyosha :: RexInput -> StateT Int IO ()
seeAlyosha inp = 
  case rexScan 0 inp of
    RexError (_, txt) -> 
        liftIO $ printf "lexical error: %s\n" (take 10 txt)
    RexEOF -> do
        n <- get 
        liftIO $ printf "'Alyosha' occurs %d times in 'karamazov.txt'\n" n
    RexToken act inp0@(_,_txt) _len inp1 -> do
        act
        seeAlyosha inp1

main :: IO ()
main = 
  withFile "./karamazov.txt" ReadMode $ \h -> do
    inp <- hGetContents h
    evalStateT (seeAlyosha $ fromString inp) 0
}
