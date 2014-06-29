module Languate.BuiltIns where

{--

This module implements an overview of the builtins and their types

Not given:

asTuple
asADT
fromADT

--}

import Languate.AST
import StdDef
import Data.Maybe


builtIns	= zip ["plus","min","mul","div","mod"] $ repeat $ [nat,nat] --> nat
getBuiltinType b	= fromMaybe (error $ "Builtin "++b++" not found") $ lookup b builtIns


(-->)	:: [Type] -> Type -> Type
(-->) t endType
	= Curry $ t ++ [endType]



(§)	:: Name -> Type
(§) nm	=  Normal nm

nat	= (§) "Nat"
int	= (§) "Int"
