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
import Languate.TAST
import Languate.Package
import Languate.TypeTable
import Languate.TypeTable.BuildKnownTypes
import Languate.TypeTable.BuildTypeLookupTable
import Languate.TypeTable.KindChecker.BuildKindTable
import Languate.TypeTable.BuildRequirementTable
import Languate.TypeTable.BuildDocstringTable
import Languate.TypeTable.BuildFreeNameTable
import Languate.TypeTable.BuildSuperTT.BuildSuperTypeTable
import Languate.TypeTable.BuildSuperTT.BuildSuperTypeTableFull
import Languate.TypeTable.BuildSuperTT.FixImplicitRequirements
import Languate.TypeTable.BuildSuperTT.ExpandFSTT

import Languate.TypeTable.Checks.CheckPackage
import Languate.TypeTable.Checks.CheckReqTable

import Languate.CheckUtils

import Data.Set as S
import Data.Map as M
import Data.Maybe
import Data.Tuple


buildTypeTable	:: Package -> Exceptions' String TypeTable
buildTypeTable w
 = inside "While building the type table" $
   do	let knownTypes	= buildKnownTypes w
	tlts		<-  buildTLTs w
	inside "While prechecking" $ validatePackage tlts w
	typeReqs	<- inside "While building the requirements table" $ buildRequirementTable w tlts knownTypes
	freeNames	<- inside "While building the free type variables name table" $ buildFreeNameTable w
	klt		<- inside "While building the kind lookup table" $ buildKindTable w tlts typeReqs freeNames
	docstrings	<- inside "While building the docstring table" $ buildDocstringTable w $ S.toList knownTypes
	supers		<- inside "While building the super type table" $ buildSuperTypeTable w tlts klt
	let fstts	= supers |> stt2fstt & fixImplicitRequirements typeReqs
	(allSupers, spareSupers)
			<- expand fstts
	inside "While checking the requirements table" $ validateReqTable klt typeReqs
	return $ TypeTable knownTypes tlts klt typeReqs supers allSupers spareSupers docstrings freeNames
