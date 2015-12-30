module Languate.BuiltIns where

{--
This module gives fixed signatures of builtin functions.
Also see TAST, where FQNs of builtin types are given.
--}

import StdDef
import Languate.TAST
import Languate.AST
import Languate.FQN


-- gives fqn of flip, which is needed to desugar sections: (:1) -> flip (:) 1
flipSign	:: (FQN, Name)
flipSign	= (toFQN' "pietervdvn:Data:ControlFlow","flip"  )
