module Languate.World (World, importGraph, buildWorld, module Languate.AliasTable)where

{--
This module provides the ''context''-datatype, which contains all commonly needed data of the currently compiling program.
--}

import StdDef
import Data.Map
import Languate.AST
import Languate.FQN
import Languate.AliasTable
import Data.Set as S
import qualified Data.Map as M

data World	= World 	{ modules	:: Map FQN Module
				, importGraph'	:: Map FQN (Set (FQN, Import))	-- this means: {module --> imports these, caused by this import statement}
				, aliasTable	:: Map FQN AliasTable	-- Alias table for each module. The aliastable contains what name maps on what module, e.g. "S" --> "Data.Set"; "AliasTable" --> "Languate.AliasTable", ... See aliastTable for more doc
				}
	deriving (Show)


importGraph	:: World -> Map FQN (Set FQN)
importGraph w	=  M.map (S.map fst) $ importGraph' w


buildWorld	:: Map FQN (Module, Set (FQN, Import)) -> World
buildWorld dict	= let 	modules		= fmap fst dict
			importGr	= fmap snd dict
			aliastTables	= buildAliasTables importGr in
			World modules importGr aliastTables
