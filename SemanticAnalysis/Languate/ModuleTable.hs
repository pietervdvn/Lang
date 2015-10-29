	module Languate.ModuleTable where

import StdDef
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

import Data.Map (Map, empty)
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


assembleModTable	:: Map FQN TypeLookupTable
				-> (Map FQN Typetable, Map FQN Typetable, Map FQN Typetable)
				-> FQN
				-> Exc ModuleTable
assembleModTable tlts (definedTTs, knownTTs, exposedTTs) fqn
	= do	let fetch dict msg	= M.lookup fqn dict ? ("No "++msg++" found for "++show fqn++", weird")
		tlt			<- fetch tlts "type lookup table"
		definedTT		<- fetch definedTTs "defined type table"
		knownTT			<- fetch knownTTs "known type table"
		exposedTT		<- fetch exposedTTs "exposed type table"
		let modCont tt		= ModuleContents tt (FunctionTable M.empty M.empty)
		return (ModuleTable tlt (modCont exposedTT) (modCont definedTT) (modCont knownTT))



mod2doc	:: (FQN,ModuleTable) -> [Doc]
mod2doc (fqn,mt)
	=  let	neededDocs	= addDocs ([exposed mt, defined mt, known mt] |> types)
					[tlt2doc ("Modules/"++ show fqn ++"/Typelookuptable for ") fqn $ typeLookupTable mt]
		linkedDocs	= neededDocs |> title |> Embed & Mu.Seq
		document	= doc ("Modules/"++show fqn++"/Moduletable for "++show fqn) "" linkedDocs in
		document : neededDocs
