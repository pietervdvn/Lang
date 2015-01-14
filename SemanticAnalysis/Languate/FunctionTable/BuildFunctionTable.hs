module Languate.FunctionTable.BuildFunctionTable where

import StdDef
import Exceptions

import Languate.CheckUtils
import Languate.TypeTable
import Languate.FQN
import Languate.World
import Languate.TAST
import Languate.AST
import Languate.FunctionTable

import Prelude hiding (lookup)
import Data.Map

-- Builds the function table for given module
buildFunctionTable	:: World -> TypeTable -> FQN ->
				Exc FunctionTable
buildFunctionTable w tt fqn
	= do	tlt	<- lookup fqn (typeLookups tt) ? ("Bug: no tlt found for "++show fqn)
		modul	<- lookup fqn (modules w) ? ("Bug: module "++show fqn ++ "not found")
		todo




knownFunctionsWithin	:: World -> FQN -> [(Name, Type)]
knownFunctionsWithin w fqn
	=  todo
