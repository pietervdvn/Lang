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

import Data.Maybe
import Data.List as L

import Control.Arrow

type UniqueConstructor	= Bool


adtDefinedFunctions	:: TypeLookupTable -> FQN -> ADTDef
				-> Exc [(Signature, Visible)]
adtDefinedFunctions tlt fqn (ADTDef name frees reqs sums _)
	= inside ("In generating the function signatures defined by "++show name) $
	  do	let definedTypeBase	= RNormal fqn name	:: RType
		let definedType	= applyTypeArgs definedTypeBase (frees |> RFree)	:: RType
		reqs	<- resolveReqs tlt reqs
		let uniqueCons	= length sums == 1
		checkNoDubbleConsnames (sums |> adtSumName)
		consDecons	<- sums |+> sumDefFunctions tlt fqn definedType reqs uniqueCons |> concat
		sums'		<- sums |+> tsum tlt
		fieldFuncs	<- buildFieldFunctions fqn definedTypeBase frees reqs sums'
		return (consDecons++fieldFuncs)


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

data TADTSum  = TADTSum
		{ tadtSumName		:: Name
             	, tadtSumVisibility	:: Visible
             	, tadtSumFields		:: [(Maybe Name, RType)]
             	}


tsum	:: TypeLookupTable -> ADTSum -> Exc TADTSum
tsum tlt (ADTSum n vis fields)
	= do	fields'	<- fields |+> onSecond (resolveType tlt)
		return $ TADTSum n vis fields'


{- note that some functions might change the type of the enclosing types:
-- a trivial container
type ID	a = {ID conts:a}
conts	: ID a -> a
conts	: (a -> b) -> ID a -> ID b
setConts	: b -> ID a -> ID b

type ID a	= {ID conts:a, NID}
conts	: ID a -> Maybe a
-- when the constructor doesn't match, nothing happens
conts	: (a -> b) -> ID a -> ID b
-- when the constructor doesn't match, nothing happens
setConts	: b -> ID a -> ID b

Arguments:

defined type (WITHOUT FREES!)
free type variable names
Requirements
{constructorName --> {fieldName --> Type}}	-- constructors with fields. Each constructor should be passed, even without fields

-}

buildFieldFunctions	:: FQN -> RType -> [Name] -> RTypeReq -> [TADTSum] -> Exc [(Signature, Visible)]
-- TODO pickup!
buildFieldFunctions fqn defType frees reqs sums
	= do	sums |+> checkNoDubblefields
		checkSameTypesVisibs' sums
		return []

{- TODO checks on fields
	- fields with the same name should have the same types and visibilities
-}



----------------- CHECKS ---------------

checkSameTypesVisibs'	:: [TADTSum] -> Check
checkSameTypesVisibs' sums
	= do	let raw = sums >>= (\(TADTSum name vis fields) -> zip fields (repeat (name, vis)))	:: [((Maybe Name, RType),(Name, Visible))]
		-- fieldname, type, consname, visibility
		let namedFields	= raw |> first (unpackFirst) |> unpackFirst & catMaybes	:: [((Name, RType),(Name, Visible))]
		-- {fieldname --> (consName, typ, vis)}
		let perField	= namedFields |> (\((field, typ), (cons,vis)) -> (field, (cons, (typ, vis))) )
					& merge	:: [(Name,[(Name, (RType, Visible))])]
		perField |+> checkSameTypesVisibs

checkSameTypesVisibs	:: (Name,[(Name, (RType, Visible))]) -> Check
checkSameTypesVisibs (fieldName, consTpsVis)
	= inside ("In the definitions of "++show fieldName) $
 	  do	let (cons, (tps, vis))	= unzip consTpsVis |> unzip
 	  	let diff	= nub tps
		let faulty	= zip cons tps & L.filter (flip elem diff . snd)
		let faulty'	= faulty |> (\(cons, tp) ->"\n"++show fieldName++" in the constructor "++show cons++"\thas the type "++show tp)
					& concat & indent
 	  	assert (length faulty == 1) $ "Fields within different constructors should have the same type, but:"++faulty'
 	  	let diffVis	= nub vis
 	  	assert (length diffVis == 1) $ "Fields within different constructors should have the same visibility, but:"++
 	  			(zip cons vis |> (\(cons, vis) -> "\n"++show fieldName++" in the constructor "++show cons++" is "++show vis )
 	  					& concat)

checkNoDubblefields	:: TADTSum -> Check
checkNoDubblefields sum
	= inside ("In the definition of the constructor "++show (tadtSumName sum)) $
	  do	let dubble	= sum & tadtSumFields |> fst & catMaybes & dubbles
	  	assert (L.null dubble) $ "Within a constructor, each fieldname should be unique.\n"++
	  		(dubble |> show & commas) ++" "++isAre (length dubble)++" used multiple times"


checkNoDubbleConsnames	:: [Name] -> Check
checkNoDubbleConsnames names
	= do	let dubble	= dubbles names
		let nr		= length dubble
		assert (null dubble) ("Constructor names should be unique, " ++
			plural nr "constructor name"++", namely "++commas (dubble |> show) ++", "++isAre nr++" used multiple times")
