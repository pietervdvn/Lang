module Languate.FunctionTable.ConstructADTFunctions where

{--
This module implements all things related to the construction of the functions, related to a new type declaration
--}

import StdDef
import Exceptions
import Languate.CheckUtils
import HumanUtils

import Languate.FQN
import Languate.AST
import Languate.TAST
import Languate.Typetable

type UniqueConstructor	= Bool


adtDefinedFunctions	:: TypeLookupTable -> FQN -> ADTDef
				-> Exc [(Signature, Visible)]
adtDefinedFunctions tlt fqn (ADTDef name frees reqs sums _)
	= do	let definedTypeBase	= RNormal fqn name	:: RType
		let definedType	= applyTypeArgs definedTypeBase (frees |> RFree)	:: RType
		reqs	<- resolveReqs tlt reqs
		let uniqueCons	= length sums == 1
		checkNoDubbleConsnames (sums |> adtSumName)
		sums |+> sumDefFunctions tlt fqn definedType reqs uniqueCons |> concat


sumDefFunctions	:: TypeLookupTable -> FQN -> RType -> RTypeReq -> UniqueConstructor -> ADTSum
			-> Exc [(Signature, Visible)]
sumDefFunctions tlt fqn defType reqs onlyCons (ADTSum name vis fields)
	= do	args	<- fields |> snd |+> resolveType tlt	-- types of the arguments
		let constr	= [buildConstructorSign fqn name reqs args defType]
		let isContstr	= if onlyCons then [] else [buildIsConstrSign fqn name reqs defType]
		let decons	= [buildDeconsSign fqn name reqs onlyCons defType args]
		let funcs	= constr ++ isContstr ++ decons
		zip funcs (repeat vis) & return

buildConstructorSign	:: FQN -> Name -> RTypeReq
				-> [RType] -> RType
				-> Signature
buildConstructorSign fqn n reqs args defType
	= Signature fqn n ([uncurriedTypes (args++[defType])],reqs)


buildIsConstrSign	:: FQN -> Name -> RTypeReq -> RType -> Signature
buildIsConstrSign fqn n reqs defType
	= Signature fqn ("is"++n) ([RCurry defType boolType],reqs)

buildDeconsSign		:: FQN -> Name -> RTypeReq -> UniqueConstructor -> RType -> [RType] -> Signature
buildDeconsSign fqn n reqs onlyCons defType rets
	= let	retTyp	= tupleTypes rets
		-- if we have a unique constructor, we don't need to wrap it in a maybe
		wrapper	= if onlyCons then id else RApplied maybeType
		in
		Signature fqn ("from"++n) ([RCurry defType (wrapper retTyp)] , reqs)


buildFieldFunctions	:: Name -> [(Name, Type)] -> [Signature]
-- TODO pickup!
buildFieldFunctions	= todo





----------------- CHECKS ---------------


checkNoDubbleConsnames	:: [Name] -> Check
checkNoDubbleConsnames names
	= do	let dubble	= dubbles names
		let nr		= length dubble
		assert (null dubble) ("Constructor names should be unique, " ++
			plural nr "constructor name"++", namely "++commas (dubble |> show) ++", "++isAre nr++" used multiple times")
