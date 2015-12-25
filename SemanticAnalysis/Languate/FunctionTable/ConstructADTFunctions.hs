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
import Data.Char (toUpper)

import Control.Arrow

type UniqueConstructor	= Bool


adtDefinedFunctions	:: TypeLookupTable -> FQN -> ADTDef
				-> Exc [(Signature, Visible, Either [Clause] [TClause])]
adtDefinedFunctions tlt fqn (ADTDef name frees reqs sums adopted)
	= inside ("In generating the function signatures defined by "++show name) $
	  do	let definedTypeBase	= RNormal fqn name	:: RType
		let definedType	= applyTypeArgs definedTypeBase (frees |> RFree)	:: RType
		reqs	<- resolveReqs tlt reqs
		let uniqueCons	= length sums + length adopted == 1
		checkNoDubbleConsnames (sums |> adtSumName)
		checkOverlappingAdoptions
		rSums		<- sums |+> resolveSum tlt
		adoptConstrs	<- adopted |+> resolveType tlt ||>> buildSumFor		-- constructors for the adopted types, to implement the type union
		consDecons	<- zip (rSums++adoptConstrs) [0..] |+> sumDefFunctions tlt fqn definedType reqs uniqueCons |> concat
		fieldFuncs	<- buildFieldFunctions fqn definedTypeBase frees reqs rSums
		(consDecons++fieldFuncs) & return


sumDefFunctions	:: TypeLookupTable -> FQN -> RType -> RTypeReq -> UniqueConstructor -> (TADTSum, Int)
			-> Exc [(Signature, Visible, Either [Clause] [TClause])]
sumDefFunctions tlt fqn defType reqs onlyCons (TADTSum name vis fields, tag)
	= inside ("In the constructor "++name) $
	  do	let args	= fields |> snd	-- types of the arguments
		checkFieldsAreNamed fields
		let constr	= [buildConstructorSign fqn name reqs args defType tag] |> second Right
		let isContstr'	= if onlyCons then [] else [buildIsConstrSign fqn name reqs defType (length fields)]
		let isContstr	= isContstr' |> second Left
		let decons	= [buildDeconsSign fqn name reqs onlyCons tag defType args] |> second Right
		let funcs	= constr ++ isContstr ++ decons	:: [(Signature, Either [Clause] [TClause])]
		let funcs'	= funcs |> (\(sign, clauses) -> (sign, vis, clauses))	:: [(Signature, Visible, Either [Clause] [TClause])]
		funcs' & return


buildConstructorSign	:: FQN -> Name -> RTypeReq
				-> [RType] -> RType -> Int
				-> (Signature, [TClause])
buildConstructorSign fqn n reqs args defType tag
	= let   sign	= Signature fqn n ([uncurriedTypes (args++[defType])],reqs)
		vars	= args |> (\a -> ([a],[])) & zip defaultFreeNames
		pats	= vars |> fst |> TAssign
		baseExpr= TApplication (signTypes sign)
				(TCall (signTypes sign) (sign {signName= "#construct"}))
				(Tag tag)
		expr	= vars |> uncurry TLocalCall & foldl simpleApply baseExpr
		in (sign, [TClause pats expr])




buildIsConstrSign	:: FQN -> Name -> RTypeReq -> RType -> Int -> (Signature, [Clause])
buildIsConstrSign fqn n reqs defType nArgs
	= let	sign	= Signature fqn ("is"++n) ([RCurry defType boolType],reqs)
		matchClause	= Clause [Deconstruct n (replicate nArgs DontCare)] (Call "True")
		failClause	= Clause [DontCare] (Call "False")
		in
		(sign, [matchClause, failClause])

buildDeconsSign		:: FQN -> Name -> RTypeReq -> UniqueConstructor -> Int -> RType -> [RType] -> (Signature, [TClause])
buildDeconsSign fqn n reqs onlyCons tag defType args
	= let	retTyp		= tupleTypes args   -- args are the arguments to the constructor, here the return types in a tuple
		-- if we have a unique constructor, we don't need to wrap it in a maybe
		wrapper		= if onlyCons then id else RApplied maybeType
		retTyp' 	= wrapper retTyp
		sign		= Signature fqn ("from"++n) ([RCurry defType (wrapper retTyp)] , reqs)
		failClause	= TClause [TDontCare] $ maybeTypeNothing retTyp'   -- only exists if multiple constructors

		deconsSign  	= sign {signName = "#deconstruct"}
		tagCheck    	= TEval $ Tag tag
		vars        	= args |> (\a -> ([a], [])) & zip defaultFreeNames
		pats		= vars |> fst |> TAssign
		tupleResult	= buildTuple args (vars |> uncurry TLocalCall)
       		successClause   = TClause [TDeconstruct deconsSign (tagCheck : pats)]
       					tupleResult
       		clauses		= if onlyCons then [successClause] else [successClause, failClause]
		in
		(sign, clauses)

data TADTSum  = TADTSum
		{ tadtSumName		:: Name
             	, tadtSumVisibility	:: Visible
             	, tadtSumFields		:: [(Maybe Name, RType)]
             	}


resolveSum	:: TypeLookupTable -> ADTSum -> Exc TADTSum
resolveSum tlt (ADTSum n vis fields)
	= do	fields'	<- fields |+> onSecond (resolveType tlt)
		return $ TADTSum n vis fields'


buildSumFor	:: RType -> TADTSum
buildSumFor rt	= TADTSum (show rt) Public [(Nothing, rt)]


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

buildFieldFunctions	:: FQN -> RType -> [Name] -> RTypeReq -> [TADTSum] -> Exc [(Signature, Visible, Either [Clause] [TClause])]
buildFieldFunctions fqn defType frees reqs sums
	= do	sums |+> checkNoDubblefields
		let perField	= perFieldname sums	:: [(Name,[((Name, Int), (RType, Visible))])]
		perField |+> checkSameTypesVisibs
		let perField'	= perField |> second perFieldname'	:: [(Name,([(Name, Int)], RType, Visible))]
		let nrOfConss	= length sums
		let lockedFrees	= buildLockedFrees reqs sums
		-- closure, as this function is a bit long to invoke!
		let buildFieldFunctionsSign'	= buildFieldFunctionsSign fqn nrOfConss defType frees lockedFrees reqs
		fieldFuncs'	<- perField' |+> (\(fieldName, (conss, rtp, vis)) ->
							do	signs <- buildFieldFunctionsSign' fieldName conss rtp
								return (signs |> (\(sign, clauses) -> (sign, vis, Left clauses ))) )
		let fieldFuncs	= concat fieldFuncs'
		return fieldFuncs


checkFieldsAreNamed	:: [(Maybe Name, RType)] -> Check
checkFieldsAreNamed fields
	= do	let unnamed = fields & L.filter (isNothing . fst) & length
		warnIf (unnamed > 4) $ "You have quite some fields in your constructor. Wouldn't you name those, boy?"

{-
Builds the signature for a given fieldname, over multiple constructors.

We assume types are the same over all constructors, this has been checked previously
Changes over the wrong constructor have no effect.

rguments:
fqn
number of contstructors; if all constructors have the same field, we can skip the maybes
the type that is declared by the ADTProd
it's free type variables
free type variables which might nog be changed, {cons name --> all frees used in this constructor}
	type X a = {X a:a}
	setA : (a -> b) -> X a -> X b -- ok
	type Y a = {Y a:a b:a}
	setA : (a -> b) -> Y a -> Y b -- not ok, what to do with field b?
	type Z a = {Z0 a:a b:a, Z1 c:a}
	setC : (a -> b) -> Z a -> Z b -- this one is fine!
-}
buildFieldFunctionsSign	:: FQN -> Int -> RType -> [Name] -> LockedFrees -> RTypeReq -> Name -> [(Name, Int)] -> RType ->
				Exc [(Signature, [Clause])]
buildFieldFunctionsSign fqn nrOfConstructors defType frees lockedFrees reqs fieldName consIndexes fieldType
	= do 	let fullType	= applyTypeArgs defType (frees |> RFree)
		let mkSign n tp	= Signature fqn n ([tp], reqs)	:: Signature
		let (conss, indexes)	= unzip consIndexes

		let mightFail	= nrOfConstructors /= length conss

		-- ## field getter
		-- wrapper: if the field occurs in all constructors, no maybe wrapping is needed
		let wrapper	= if mightFail then RApplied maybeType
					else id
		let justWrap e	= if mightFail then Seq [Call "Just", e] else e

		let failClause	= Clause [DontCare] (Call "Nothing")

		let getT	= RCurry fullType (wrapper fieldType)
		let getPats i
				= (replicate i DontCare) ++ [Assign (head defaultFreeNames), MultiDontCare]
		let genClause (consName, fieldIndex)
				= Clause [Deconstruct consName (getPats fieldIndex)]
					(justWrap $ Call $ head defaultFreeNames)
		let getClauses	= (consIndexes |> genClause) ++ if mightFail then [failClause] else []

		-- ## field modifier function
		-- free types in this field
		let fieldTypeFrees	= freesInRT fieldType
		-- what frees can we freely change? (pun intended)
		let unchangeable'	= consFrees lockedFrees & L.filter ((`elem` conss) . fst) >>= snd
		let unchangeable	= unchangeable' ++ freesInConstraints lockedFrees	:: [Name]
		let changeable	= fieldTypeFrees L.\\ unchangeable
		let newFreeNames	= defaultFreeNames & L.filter (`notElem` frees) & take (length changeable)
		let mapping	= zip changeable newFreeNames
		let newFrees	= frees |> (\a -> fromMaybe a $ L.lookup a mapping)
		newFieldTyp	<- subs (mapping ||>> RFree) fieldType
		newDefTyp	<- subs (mapping ||>> RFree) defType
		newReqs'	<- subsReq mapping reqs
		-- of course, both the old and new constraints are still in effect!
		let newReqs	= reqs ++ newReqs'
		-- actual construction of the change function signatures
		let defType'	= applyTypeArgsFree defType frees
		let newDefType'	= applyTypeArgsFree newDefTyp newFrees
		let modifierT	= RCurry (RCurry fieldType newFieldTyp) (RCurry defType' newDefType')

		let modifierClause	= []	-- TODO

		-- ## Setter
		let setterName	= "set"++ [toUpper (head fieldName)] ++ tail fieldName
		let setterT	= RCurry newFieldTyp (RCurry defType' newDefType')
		let setterClause	= []	-- TODO

		return [(mkSign fieldName getT, getClauses), (mkSign fieldName modifierT, modifierClause), (mkSign setterName setterT, setterClause)]

{- chops and dices {constructor --> (attached type, visibility)}
 into [constructors], (attached type, visibility)
-}
perFieldname'	:: [((Name, Int), (RType, Visible))] -> ([(Name, Int)],RType, Visible)
perFieldname' vals
	= vals 	& unzip	-- ([(Name, Int)],[RType, Visible])
		|> head	-- ([Name, Int], (RType, Visible))	-- we throw away the rtype/visibility data, as these are all the same
		& (\(conss, (rt, vis)) -> (conss, rt, vis))


-- chops and dices TADTsums into {fieldName --> {constructor --> (attached type, visibility, index of the field)}}
perFieldname	:: [TADTSum] -> [(Name,[( (Name, Int), (RType, Visible))])]
perFieldname sums
	= let	raw = sums >>= (\(TADTSum name vis fields) -> zip fields (repeat (name, vis)))	:: [((Maybe Name, RType),(Name, Visible))]
		-- fieldname, type, consname, visibility
		namedFields = raw |> first unpackFirst		-- :: [(Maybe (Name, RType),(Name, Visible))] {-
				|> unpackFirst			-- :: [ Maybe ( (Name, RType), (Name, Visible)  )] {-
				& zip [0..]			-- :: [(Int, Maybe ( (Name, RType), (Name, Visible)  )) ] {-
				|> unpackSecond			-- :: [Maybe (Int, ( (Name, RType), (Name, Visible)  ))]  {-
				& catMaybes			:: [ (Int, ((Name, RType), (Name, Visible)) ) ] --}

		reWrite (index, ( (fieldName, fieldType), (consName, visibility) ) )
			= (fieldName, ((consName, index), (fieldType, visibility)) )

		-- {fieldname --> (consName, typ, vis)}
		perField	= namedFields |> reWrite
						:: [ (Name, ((Name, Int) , (RType, Visible)) ) ] in
		perField & merge


data LockedFrees
	= LockedFrees { freesInConstraints	:: [Name]	-- frees which are used in other constraints. Should never be changed by the fields
			, consFrees		:: [(Name, [Name])]	-- what constructor uses what frees? Frees are NOT nubbed!
			}
	deriving (Show)


buildLockedFrees	:: RTypeReq -> [TADTSum] -> LockedFrees
buildLockedFrees rtreqs sums
	=  let	lockedInReq	= rtreqs >>= snd >>= freesInRT
		-- {consname --> [ [frees, in, field 1], [frees, in, field 2] ]}
		fieldFrees	= sums 	|> (tadtSumName &&& tadtSumFields)
					|||>>> (freesInRT . snd)
					:: [(Name, [[Name]])]
		-- frees are locked if, for the given constructor, the free occurs in multiple fields
		-- each sublist is nubbed -> we concat, dubbles occur more!
		-- [[Name]] -> [Name]
		lockedFreesIn l	= l |> nub & concat & dubbles
		lockedFrees	= fieldFrees ||>> lockedFreesIn
		in
		LockedFrees lockedInReq lockedFrees


----------------- CHECKS ---------------

checkSameTypesVisibs	:: (Name,[((Name, Int), (RType, Visible))]) -> Check
checkSameTypesVisibs (fieldName, consTpsVis)
	= inside ("In the definitions of "++show fieldName) $
 	  do	let (cons', (tps, vis))	= unzip consTpsVis |> unzip
 	  	let cons	= cons' |> snd
 	  	let diff	= nub tps
		let faulty	= zip cons tps & L.filter (flip elem diff . snd)
		let faulty'	= faulty |> (\(cons, tp) ->"\n"++show fieldName++" in the constructor "++show cons++"\thas the type "++show tp)
					& concat & indent
 	  	assert (length diff == 1) $ "Fields within different constructors should have the same type, but:"++faulty'
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

checkOverlappingAdoptions	:: Check
checkOverlappingAdoptions	= pass	-- TODO FIXME TODO: check wether adopted types don't overlap
