	module Languate.ModuleTable where

import StdDef
import Languate.MarkUp as Mu
import Exceptions
import Languate.CheckUtils

import Languate.TAST
import Languate.AST hiding (functions)

import Languate.Package
import Languate.FQN
import Languate.PrecedenceTable

import Languate.Typetable.TypeLookupTable
import Languate.Typetable
import Languate.FunctionTable

import Data.Map (Map, empty)
import qualified Data.Map as M

{--
The moduletable contains all information that is local to a module
--}

data ModuleTable
	= ModuleTable	{
		typeLookupTable	:: TypeLookupTable,
		types		:: Typetable,
		functions	:: FunctionTable
		} deriving (Show)


assembleModTable	:: Map FQN TypeLookupTable
				-> Map FQN Typetable
				-> Map FQN FunctionTable
				-> FQN
				-> Exc ModuleTable
assembleModTable tlts tts fts fqn
	= do	let fetch dict msg	= M.lookup fqn dict ? ("No "++msg++" found for "++show fqn++", weird")
		tlt			<- fetch tlts "type lookup table"
		tt			<- fetch tts "type table"
		ft			<- fetch fts "function table"
		return (ModuleTable tlt tt ft)



mod2doc	:: (FQN,ModuleTable) -> [Doc]
mod2doc (fqn,mt)
	=  let	modulePath fqn	= "Module/"++show fqn++"/"
		neededDocs	= uncurry (:) (functiontable2doc modulePath fqn $ functions mt) ++
				  uncurry (:) (typetable2doc modulePath fqn $ types mt) ++
					[tlt2doc (modulePath fqn ++"Typelookuptable for ") fqn $ typeLookupTable mt]
		linkedDocs	= neededDocs |> title |> Embed & Mu.Seq
		document	= doc (modulePath fqn++"Moduletable for "++show fqn) "" linkedDocs in
		document : neededDocs
