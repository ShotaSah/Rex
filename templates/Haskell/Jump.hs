
data RexLastAcc act = 
    RexLccNone
  | RexLccSkip !RexInput
  | RexLcc act !RexInput !Int !RexInput

data RexReturn act = 
    RexError !RexInput
  | RexEOF
  | RexToken act !RexInput !Int !RexInput
