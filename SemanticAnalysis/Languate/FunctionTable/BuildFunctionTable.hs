module Languate.FunctionTable.BuildFunctionTable where

import StdDef

import Languate.AST
import Languate.TAST
import Languate.FQN
import Languate.Package
import Languate.TypeTable

import Languate.FunctionTable

import Data.Map

buildFunctionTables	:: Package -> TypeTable -> FunctionTables
buildFunctionTables p tt
	= mapWithKey (buildFunctionTable p tt) $ modules p



buildFunctionTable		:: Package -> TypeTable -> FQN -> Module -> FunctionTable
buildFunctionTable p tt fqn m	= FunctionTable $ fromList [("f", ([RFree "a",RFree "b"], []))]
