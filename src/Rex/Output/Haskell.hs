module Rex.Output.Haskell (
    moduleToHsSource
  ) where

import Rex.Module (RexModule)
import Rex.Automata.DFA (DFA)
import qualified Rex.Data.Alpha as A
import qualified Rex.Output.Haskell.Jump as J (moduleToHsSource)


moduleToHsSource :: A.Impl enc => RexModule (DFA enc) -> String
moduleToHsSource = J.moduleToHsSource

