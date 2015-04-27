module Languate.MaintenanceAccess.TestBuild where

{--
This module builds all the stuff!
--}

import qualified Bnf
import Exceptions


import Languate.File2Package
import Languate.FQN
import Languate.AST
import Languate.Package
import Languate.MarkUp

import System.IO.Unsafe
import System.Directory


import Languate.TableOverview

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

import Data.Map
import StdDef

bnfs		= unsafePerformIO $ Bnf.load "../Parser/bnf/Languate"
path		= "../workspace/Data"
packageIO	= loadPackage' bnfs (toFQN' "pietervdvn:Data:Prelude")

t	= do
	w	<- packageIO path
	dir	<- getCurrentDirectory
	print $ buildKnownTypes w
	let knownTypes	= buildKnownTypes w
	tlts		<- rio $ buildTLTs w
	rio $ inside "While prechecking" $ validatePackage tlts w
	typeReqs	<- rio $ inside "While building the requirements table" $ buildRequirementTable w tlts knownTypes
	freeNames	<- rio $ inside "While building the free type variables name table" $ buildFreeNameTable w
	klt		<- rio $ inside "While building the kind lookup table" $ buildKindTable w tlts typeReqs freeNames
	docstrings	<- rio $ inside "While building the docstring table" $ buildDocstringTable w $ S.toList knownTypes
	supers		<- rio $ inside "While building the super type table" $ buildSuperTypeTable w tlts klt
	let fstts	= supers |> stt2fstt & fixImplicitRequirements typeReqs
	putStrLn "FSTTS "
	(allSupers, spareSupers)
			<- rio $ expand klt fstts
	putStrLn "Done"
	rio $ inside "While checking the requirements table" $ validateReqTable klt typeReqs
	putStrLn "Done0"



rio e	= do	e' 	<- runExceptionsIO' e
		return e'
