
import UTF8Wrapper


data RexLastAcc act = 
    RexLccNone
  | RexLccSkip !RexInput
  | RexLcc act !RexInput !Int !RexInput

data RexReturn act = 
    RexError !RexInput
  | RexEOF
  | RexToken act !RexInput !Int !RexInput

rex_action_0 = 
     putStrLn "abcde" 

rex_action_1 = 
     putStrLn "abcd" 

rex_action_2 = 
     putStrLn "abc" 

-- rexScan :: Int -> RexInput -> RexReturn _
rexScan sc inp0 = 
    case state_0 RexLccNone 0 inp0 of
    (RexLccNone,inp1) -> case rexGetChar 0 inp1 of
        Just {} -> RexError inp1
        Nothing -> RexEOF
    (RexLccSkip inp1,_) -> rexScan sc inp1
    (RexLcc act inp0 len inp1,_) -> RexToken act inp0 len inp1
  where
    state_0 acc len0 inp0 = acc `seq` len0 `seq` inp0 `seq`
        shift_0 acc len0 inp0
    
    shift_0 acc len0 inp0 = acc `seq` len0 `seq` inp0 `seq`
        case rexGetChar len0 inp0 of
        Just (c,len1,inp1) -> (\next -> next acc len1 inp1) $ case c of
            97 -> state_4
            _ -> \_ _ _ -> (acc,inp0)
        Nothing -> (acc,inp0)
    
    state_1 acc len0 inp0 = acc `seq` len0 `seq` inp0 `seq`
        (accept_1 len0 inp0, inp0)
    
    state_2 acc len0 inp0 = acc `seq` len0 `seq` inp0 `seq`
        shift_2 (accept_2 len0 inp0) len0 inp0
    
    shift_2 acc len0 inp0 = acc `seq` len0 `seq` inp0 `seq`
        case rexGetChar len0 inp0 of
        Just (c,len1,inp1) -> (\next -> next acc len1 inp1) $ case c of
            101 -> state_1
            _ -> \_ _ _ -> (acc,inp0)
        Nothing -> (acc,inp0)
    
    state_3 acc len0 inp0 = acc `seq` len0 `seq` inp0 `seq`
        shift_3 (accept_3 len0 inp0) len0 inp0
    
    shift_3 acc len0 inp0 = acc `seq` len0 `seq` inp0 `seq`
        case rexGetChar len0 inp0 of
        Just (c,len1,inp1) -> (\next -> next acc len1 inp1) $ case c of
            100 -> state_2
            _ -> \_ _ _ -> (acc,inp0)
        Nothing -> (acc,inp0)
    
    state_4 acc len0 inp0 = acc `seq` len0 `seq` inp0 `seq`
        shift_4 acc len0 inp0
    
    shift_4 acc len0 inp0 = acc `seq` len0 `seq` inp0 `seq`
        case rexGetChar len0 inp0 of
        Just (c,len1,inp1) -> (\next -> next acc len1 inp1) $ case c of
            98 -> state_5
            _ -> \_ _ _ -> (acc,inp0)
        Nothing -> (acc,inp0)
    
    state_5 acc len0 inp0 = acc `seq` len0 `seq` inp0 `seq`
        shift_5 acc len0 inp0
    
    shift_5 acc len0 inp0 = acc `seq` len0 `seq` inp0 `seq`
        case rexGetChar len0 inp0 of
        Just (c,len1,inp1) -> (\next -> next acc len1 inp1) $ case c of
            99 -> state_3
            _ -> \_ _ _ -> (acc,inp0)
        Nothing -> (acc,inp0)
    
    accept_1 len inp = 
        case (sc :: Int) of
        0 -> action_0 len inp
        _ -> error "rexScan: undefined start code"
    
    accept_2 len inp = 
        case (sc :: Int) of
        0 -> action_1 len inp
        _ -> error "rexScan: undefined start code"
    
    accept_3 len inp = 
        case (sc :: Int) of
        0 -> action_2 len inp
        _ -> error "rexScan: undefined start code"
    
    action_0 len inp1 = 
        RexLcc rex_action_0 inp0 len inp1
    
    action_1 len inp1 = 
        RexLcc rex_action_1 inp0 len inp1
    
    action_2 len inp1 = 
        RexLcc rex_action_2 inp0 len inp1
    

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
main = seeAbcs $ fromString "abcdeabcabcdabcabcdeabcabcdabc"


