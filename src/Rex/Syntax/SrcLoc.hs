module Rex.Syntax.SrcLoc (
    SrcLoc(..)
  , Located(..)
  ) where


data SrcLoc = SrcLoc {
    locLine :: !Int
  , locColumn :: !Int
  } deriving Show


data Located a = L SrcLoc a

