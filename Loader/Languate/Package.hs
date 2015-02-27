module Languate.Package (Package, modules, importGraph', importGraph, aliasTables, aliasLookupTables, buildWorld, module Languate.AliasLookupTable, module Languate.AliasTable)where

{--
This module provides the ''context''-datatype, which contains all commonly needed data of the currently compiling program.
--}

import StdDef
import Data.Map
import Languate.AST
import Languate.FQN
import Languate.AliasLookupTable
import Languate.AliasTable
import Data.Set as S
import qualified Data.Map as M

data Package	= Package 	{ modules	:: Map FQN Module
				, importGraph'	:: Map FQN (Map FQN Import)	-- this means: {module --> imports these, caused by this import statement}
				, aliasLookupTables	:: Map FQN AliasLookupTable	-- Alias table for each module. The aliastable contains what name maps on what module, e.g. "S" --> "Data.Set"; "AliasTable" --> "Languate.AliasTable", ... See aliastTable for more doc
				, aliasTables	:: Map FQN AliasTable
				}
	deriving (Show)


importGraph	:: Package -> Map FQN (Set FQN)
importGraph	=  M.map (S.fromList . keys) . importGraph'


buildWorld	:: Map FQN (Module, Set (FQN, Import)) -> Package
buildWorld dict	= let 	modules		= fmap fst dict
			importGr	= fmap (merge . S.toList . snd) dict	:: Map FQN [(FQN, [Import])]
			importGr'	= fmap (M.fromList . fmap unp) importGr	:: Map FQN (Map FQN Import)
			importSet	= fmap (S.fromList . M.toList) importGr'
			aliastLookupTables	= buildAliasLookupTables importSet
			aliasTables	= buildAliasTables importSet in
			Package modules importGr' aliastLookupTables aliasTables
	where 	unp	(fqn, [imp])	= (fqn, imp)
		unp	(fqn, imps)	= error $ "Warning: double import. "++show fqn++" is imported by two or more import statements"
