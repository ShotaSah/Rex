module Rex.Output.C (
    moduleToCSource
  ) where

import Rex.Module (RexModule)
import Rex.Automata.DFA (DFA)
import qualified Rex.Data.Alpha as A
import qualified Rex.Output.C.Jump as J (moduleToCSource)


moduleToCSource :: A.Impl enc => RexModule (DFA enc) -> String
moduleToCSource = J.moduleToCSource

