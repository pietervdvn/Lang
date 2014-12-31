module Languate.TypeTable.BuildTypeTable where

{--
This module builds the type table for the given module. This is done in several steps:


Building of the TYPE LOOKUP TABLE
=================================

We build the Type Lookup Tables for each module (to resolve types)
We now have a clear sight which module can view what types, and where this type was originally implemented.

Then, we build the type requirements table.


------

https://www.haskell.org/haskellwiki/GADTs_for_dummies

type synonyms act as functions, with kinds we can do arity checks
data defs and class defs as new declarations (no kind magic here, just simple declarations)
instance defs for predicates
subtype defs for predicates

Type Requirements are passed when needed and implicit

--}


import StdDef
import Exceptions
import Languate.FQN
import Languate.World
import Languate.TypeTable
import Languate.TypeTable.BuildRequirementTable
import Languate.TypeTable.BuildTypeLookupTable
import Languate.Checks

import Data.Map
import Data.Map as M

buildTypeTable	:: World -> Exceptions' String (Map FQN TypeLookupTable, TypeTable)
buildTypeTable w
		= do	let tlts	=  buildTLTs w
			validateWorld tlts w
			typeReqs	<- buildRequirementTables tlts w |> M.elems |> M.unions
			return $ ( tlts , TypeTable (todos "Kinds") typeReqs (todos "supertypes") (todos "instConstr") )
