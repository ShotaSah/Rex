
data RexAcc act = 
    RexAccNone
  | RexAccSkip
  | RexAcc act

data RexLastAcc act = 
    RexLccNone
  | RexLccSkip !RexInput
  | RexLcc act !RexInput !Int !RexInput

data RexReturn act = 
    RexError !RexInput
  | RexEOF
  | RexToken act !RexInput !Int !RexInput

-- rexScan :: Int -> RexInput -> RexReturn _
rexScan sc inp0 = 
    case state sc RexAccNone 0 inp0 of
    (RexLccNone,inp1) -> case rexGetChar 0 inp1 of
        Just {} -> RexError inp1
        Nothing -> RexEOF
    (RexLccSkip _ inp1,_) -> rexScan sc inp1
    (RexLcc _ act len inp1) -> RexToken act inp0 len inp1
  where
    state s acc len inp1 = acc `seq` len `seq` inp1 `seq`
        let 
            acc' = case {- rex_accept s -} of
                RexAccNone -> acc
                RexAccSkip -> RexLccSkip inp1
                RexAcc act -> RexLcc act inp0 len inp1
         in shift s acc' len inp1

    shift s acc len0 inp1 = acc `seq` len0 `seq` inp1 `seq`
        case rexGetChar len0 inp1 of
        Just (c,len1,inp2) | c >= 0 -> 
            let 
                s' = {- rex_table s c -}
             in state s' acc len1 inp2
        _ -> (acc,inp1)
