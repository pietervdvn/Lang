module Languate.KindChecker.BuildKindTable where

{--
This module implements the wrappers to build the actual kind table.
--}

import Exceptions

import Languate.FQN
import Languate.TypeTable
import Languate.World
import Languate.KindChecker.ConstructKindConstraints
import Languate.KindChecker.Solver

import Languate.Checks.CheckUtils

import Data.Map (Map)


-- Builds the kind table, does lots of checks on it
buildKindTable	:: World -> Map FQN TypeLookupTable -> Exceptions' String KindLookupTable
buildKindTable w tlts
		= do	constraints	<- inside ("While gathering the kind constraints ") $ buildKindConstraints tlts w
			inside ("While solving the kind constraints") $ solve constraints
			-- Thats it folks! No more code here
