	module Languate.ModuleTable where

import StdDef
import Data.Map
import Languate.MarkUp as Mu
import Exceptions
import Languate.CheckUtils

import Languate.TAST
import Languate.AST

import Languate.Package
import Languate.FQN
import Languate.PrecedenceTable

import Languate.Typetable.TypeLookupTable
import Languate.Typetable

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
		types		:: Typetable,
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
		tt		<- buildTypetable mod tlt fqn
		let emptieFT	= FunctionTable empty empty
		let emptie	= ModuleContents (Typetable empty) emptieFT
		return $ ModuleTable tlt (ModuleContents tt emptieFT) emptie emptie




mod2doc	:: (FQN,ModuleTable) -> [Doc]
mod2doc (fqn,mt)
	=  let	neededDocs	= addDocs ([exposed mt, defined mt, known mt] |> types)
					[tlt2doc ("Modules/"++ show fqn ++"/Typelookuptable for ") fqn $ typeLookupTable mt]
		linkedDocs	= neededDocs |> title |> Embed & Mu.Seq in
		[doc ("Modules/"++show fqn++"/Moduletable for "++show fqn) "" $ linkedDocs] ++ neededDocs
