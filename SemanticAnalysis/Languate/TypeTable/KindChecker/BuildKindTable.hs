module Languate.TypeTable.KindChecker.BuildKindTable where

{--
This module implements the wrappers to build the actual kind table.
--}

import StdDef
import Exceptions

import Languate.FQN
import Languate.TypeTable
import Languate.World
import Languate.TypeTable.KindChecker.ConstructKindConstraints
import Languate.TypeTable.KindChecker.Solver

import Languate.CheckUtils

import Data.Map (Map)


-- Builds the kind table, does lots of checks on it
buildKindTable	:: World -> Map FQN TypeLookupTable -> TypeReqTable -> Map TypeID (Map Int Name) -> Exceptions' String KindLookupTable
buildKindTable w tlts treqs freeNm
		= do	constraints	<- inside "While gathering the kind constraints " $ buildKindConstraints tlts w
			inside "While solving the kind constraints" $ solve treqs freeNm constraints
			-- Thats it folks! No more code here
