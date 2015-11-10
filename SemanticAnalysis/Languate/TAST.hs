module Languate.TAST (module TAST) where

{--

This module the TypeChecked-AST, the 'simpler' version of Languate.AST. It is the counterpart of Languate.AST.

In these data structure, all source-code things, docs, ... are stripped of. Only the typed program remains, which will be interpreted later.

It is this data-structure that all semantic analysis things use or build.

--}

import Languate.TAST.Defaults as TAST
import Languate.TAST.DefBuiltinTypes as TAST
import Languate.TAST.DefExpr as TAST
import Languate.TAST.DefType as TAST
import Languate.TAST.ExprUtils as TAST
import Languate.TAST.KindUtils as TAST
import Languate.TAST.TypeUtils as TAST


{-


-- simple conversion, only use for type declarations. E.g. '''data Bool = ...''' in module '''Data.Bool''': '''asRType (FQN ["Data"] "Bool")(Normal "Bool") '''
asRType	:: FQN -> Type -> RType
asRType fqn (Normal [] nm)
	= RNormal fqn nm

asRType'	:: (FQN, Name) -> RType
asRType' (fqn, nm)
		= RNormal fqn nm


-}
