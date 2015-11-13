module Languate.FunctionTable.ModuleTraverser where

{--
This module traverser the module, to fetch function statements
--}

import StdDef
import Exceptions
import Languate.CheckUtils

import Languate.FQN
import Languate.AST
import Languate.TAST
import Languate.Typetable
import Languate.FunctionTable.ConstructADTFunctions
import Languate.FunctionTable.Def

import Control.Arrow


-- fetches the function signatures that are defined within the statement
definedFuncSign	:: Module -> TypeLookupTable -> FQN -> Statement -> Exc [(Signature, (Visible, Generated))]
definedFuncSign m tlt fqn (FunctionStm function)
	= do	defs	<- signs function |+> resolveSignature tlt fqn
		visibs	<- defs |+> getVisibility m (visibility function)
		let visibsGen	= zip visibs (repeat False)
		return (zip defs visibsGen)
definedFuncSign m tlt fqn (ADTDefStm adtDef)
	= adtDefinedFunctions tlt fqn adtDef |||>>> (id &&& const True)
definedFuncSign _ _ _ _
	= return []
