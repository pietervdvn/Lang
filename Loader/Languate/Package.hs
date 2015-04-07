module Languate.Package (Package, modules, importGraph', importGraph, aliasTables, aliasLookupTables, buildWorld, module Languate.AliasLookupTable, module Languate.AliasTable)where

{--
This module provides the ''context''-datatype, which contains all commonly needed data of the currently compiling program.
--}

import StdDef
import Exceptions
import Languate.CheckUtils

import Data.Map
import Languate.AST
import Languate.FQN
import Languate.AliasLookupTable
import Languate.AliasTable
import Data.Set as S
import qualified Data.Map as M
import Languate.Manifest (Manifest)

data Package	= Package 	{ manifest	:: Manifest
				, modules	:: Map FQN Module
				, importGraph'	:: Map FQN (Map FQN Import)	-- this means: {module --> imports these, caused by this import statement}
				, aliasLookupTables	:: Map FQN AliasLookupTable	-- Alias table for each module. The aliastable contains what name maps on what module, e.g. "S" --> "Data.Set"; "AliasTable" --> "Languate.AliasTable", ... See aliastTable for more doc
				, aliasTables	:: Map FQN AliasTable
				}
	deriving (Show)


importGraph	:: Package -> Map FQN (Set FQN)
importGraph	=  M.map (S.fromList . keys) . importGraph'


buildWorld	:: Manifest -> Map FQN (Module, Set (FQN, Import)) -> Exc Package
buildWorld manifest dict = do
	let modules		= fmap fst dict
	let importGr	= fmap (merge . S.toList . snd) dict & M.toList
				:: [(FQN, [(FQN, [Import] )])]
	cleanImportGr	<- mapM (\(fqn, imps) -> inFile fqn $ do
							imps'	<- mapM unp imps
							return (fqn, imps')) importGr
	let importGr'	= M.fromList cleanImportGr |> M.fromList
	let importSet	= fmap (S.fromList . M.toList) importGr'
	let aliastLookupTables	= buildAliasLookupTables importSet
	let aliasTables	= buildAliasTables importSet
	return $ Package manifest modules importGr' aliastLookupTables aliasTables
	where 	unp	(fqn, [imp])	= return (fqn, imp)
		unp	(fqn, imps)	= do	warn $ "Error: double import. "++show fqn++" is imported by two or more import statements"
						return (fqn, head imps)
			-- TODO make a non-crashing warning from this
