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
import Languate.PrecedenceTable

import Languate.Typetable.TypeLookupTable

import Data.Map (Map)
import qualified Data.Map as M

{--
The moduletable contains all information that is local to a module
--}

data ModuleTable
	= ModuleTable	{
		typeLookupTable	:: TypeLookupTable,
		exposed		:: ModuleContents,
		defined		:: ModuleContents,
		known		:: ModuleContents
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



buildModuleTable	:: Package -> PrecedenceTable -> Map FQN TypeLookupTable -> FQN -> Module
				-> Exc ModuleTable
buildModuleTable p precT tlts fqn mod
	= do	tlt		<- M.lookup fqn tlts ? ("No typelookup table found for "++show fqn++".Are the fqns really the same?")
		let emptie	= ModuleContents $ FunctionTable empty empty
		return $ ModuleTable tlt emptie emptie emptie



mod2doc	:: (FQN,ModuleTable) -> [Doc]
mod2doc (fqn,mt)
	=  [doc ("Modules/"++show fqn++"/Moduletable for "++show fqn) "" $ Base "hi", tlt2doc ("Modules/"++ show fqn ++"/Typelookuptable for ") fqn $ typeLookupTable mt]
