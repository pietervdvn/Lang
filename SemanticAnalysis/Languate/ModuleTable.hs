module Languate.ModuleTable where

import StdDef
import Data.Map
import Languate.MarkUp
import Exceptions
import Languate.CheckUtils

import Languate.TAST
import Languate.AST

import Languate.Package
import Languate.FQN
import Languate.Precedence.PrecedenceTable

{--
The moduletable contains all information that is local to a module
--}

data ModuleTable
	= ModuleTable	{
		exposed	:: ModuleContents,
		defined	:: ModuleContents,
		known	:: ModuleContents
		} deriving (Show)


data ModuleContents
	= ModuleContents {
		functions	:: FunctionTable
		} deriving (Show)

data FunctionTable
	= FunctionTable {
		implementations	:: Map Signature [TClause],
		documentation	:: Map Signature (String, [Law])
		} deriving (Show)



buildModuleTable	:: Package -> PrecedenceTable ->  FQN -> Module
				-> Exc ModuleTable
buildModuleTable p precT fqn mod
	= do	let emptie	= ModuleContents $ FunctionTable empty empty
		return $ ModuleTable emptie emptie emptie



mod2doc	:: (FQN,ModuleTable) -> [Doc]
mod2doc (fqn,mt)
	=  [doc ("Moduletable for "++show fqn) "" $ Base "hi"]
