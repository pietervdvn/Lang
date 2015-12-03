module Languate.TypeConstraint.Utils where

import StdDef
import Exceptions
import Languate.CheckUtils

import Languate.TAST
import Languate.Typetable.TypetableDef
import Languate.TypeConstraint.Def

import Data.List as L
import Data.Set as S
import Data.Map as M

import Data.Maybe


isConstraintMet	:: Typetable -> TypeConstraint -> Exc Bool
isConstraintMet tt constr
	= isConstraintMet' tt S.empty constr


isConstraintMet'	:: Typetable -> Set TypeConstraint -> TypeConstraint -> Exc Bool
isConstraintMet' tt met constraint
 | constraint `S.member` met
 		= return True
 | otherwise	= inside ("Where constraints are met: "++show met) $
 		  do	extraNeeded	<- neededConstraints tt met constraint
 			case extraNeeded of
 				Nothing	-> return False
 				(Just moreConstraints)
 					-> moreConstraints |+> isConstraintMet' tt (S.insert constraint met)
 							|> and



{-
Binds a type in another type.
E.g.
A -> B `bind` a -> b
is only possible if "a == A", "b == B"
This gives a lot of type constraints, which might be solved.
Note that these have to be solved even further to see if it is actually possible
-}
bnd	:: RType -> RType -> TypeConstraint
-- Asylum Cat is a subtype of Asylum animal
-- Just as List' a is a subtype of List a
-- and List' Cat is a subtype of List Animal
bnd t0@(RApplied base0 arg0) t1@(RApplied base1 arg1)
	=  choose t0 t1
		[bind base0 base1, bind arg0 arg1]
-- bind (Nat -> _) in (Nat' -> _) should work, as the input of the proposed function is bigger then the one of the wanted function
-- bind (_ -> Nat') in (_ -> Nat) should work, as the result of the proposed function is smaller then the one of we want
bnd t0@(RCurry inputType0 outputType0) t1@(RCurry inputType1 outputType1)
	= choose t0 t1
		[bind inputType1 {-yes, reversed here-} inputType0, bind outputType0 outputType1]
bnd t (RConj tps)
	= tps |> bind t & All	-- t should be all of the tps
bnd (RConj tps) t
	= tps |> (`bind` t) & foldl1 Choose
bnd sub super
	= SubTypeConstr sub super



bind	:: RType -> RType -> TypeConstraint
bind t0 t1
 | t0 == t1	= All []
 | otherwise	= bnd t0 t1




choose	:: RType -> RType -> [TypeConstraint] -> TypeConstraint
choose t0 t1 conss
	= Choose (All conss) (SubTypeConstr t0 t1)


{-
Given two types, checks wether the first is a subtype of the second
Types must not contain:
- conjunctions
- free types (at a top level)
- curries in the first argument
In other words, the first argument should be normal
-}
isSuper	:: Typetable -> Set TypeConstraint -> RType -> RType -> Exc (Maybe TypeConstraint)
isSuper tt met t0 t1
 | t0 == t1	= return $ Just $ All []
 | (SubTypeConstr t0 t1) `S.member` met
 		= return $ Just $ All []
 | isRFree t0	= return $ Just $ SubTypeConstr t0 t1
 | otherwise	= _isSuper tt t0 t1


_isSuper	:: Typetable -> RType -> RType -> Exc (Maybe TypeConstraint)
_isSuper tt sub super
 | isNormal sub
	= do	-- check kinds first, they should both have the normal kind!
		subKind	<- simpleKindOf tt (const $ return Kind) sub
		superKind	<- simpleKindOf tt (const $ return Kind) super
		if subKind /= Kind || superKind /= Kind then do
			return Nothing
		else do
		-- let's dissassemble t0
 		let (base, args)	= dissassemble sub
		ti	<- getTi tt base
		-- we build a mapping for each free
		-- {a0 -> first arg, ...}
		let mapping	= zip defaultFreeNames args
		possibleSupers	<- applicableSupers ti super |+> subsSuper mapping
		let possibleConstraints	= possibleSupers|> testSuper super
		return $ if L.null possibleConstraints then Nothing
				else Just $ foldl1 Choose possibleConstraints
 | otherwise
 	= do	warn $ "Trying to see "++show sub++" as subtype of "++show super
 		return $ Nothing

{-
Given
A a b c
and
A d e f,
what constraints are imposed to let A a b c be in A d e f

-}
testSuper	:: RType -> (RType, [TypeConstraint]) -> TypeConstraint
testSuper wantedForm (candidate, constraints)
	= All $ bind candidate wantedForm:constraints & L.filter (not . isTrivialConstraint)




{-
Gets supers out of type-info which are based on RType
-}
applicableSupers	:: TypeInfo -> RType -> [(RType, [TypeConstraint])]
applicableSupers ti t
 | not (isNormal t)
 	= supertypes ti & M.toList & L.filter (not . isNormal . fst)
 | otherwise
 	= let	allSupers	= supertypes ti & M.toList & L.filter (isNormal . fst)
 		base		= getBaseTID t
 		in
 		allSupers & L.filter ((==) base . getBaseTID . fst)



{-
Given a type constraints, gives the constraints that are needed to meet this constraints.
Returns nothing if this constraint can never be met in the current type table
-}
neededConstraints	:: Typetable -> Set TypeConstraint -> TypeConstraint -> Exc (Maybe [TypeConstraint])
neededConstraints tt met (SubTypeConstr sub super)
	= isSuper tt met sub super ||>> (:[])
neededConstraints tt met (All constrs)
	= do	constrs'	<- constrs |+> neededConstraints tt met	:: Exc [Maybe [TypeConstraint]]
		let mconstrs	= sequence constrs' |> concat	:: Maybe [TypeConstraint]
		return mconstrs
neededConstraints tt met (Choose a b)
	= do	a'	<- neededConstraints tt met a	:: Exc (Maybe [TypeConstraint])
		b'	<- neededConstraints tt met b	:: Exc (Maybe [TypeConstraint])
		if isJust a' && isJust b' then
			return $ Just $ [Choose (All $ fromJust a') (All $ fromJust b')]
		else return $ firstJust a' b'

{-Recursively build all needed constraints to meet all the wanted constraints -}
allNeededConstraints	:: Typetable -> Set TypeConstraint -> [TypeConstraint] -> Exc (Maybe (Set TypeConstraint))
allNeededConstraints tt met wanted
	= _allNeededConstraints tt met S.empty wanted


_allNeededConstraints	:: Typetable -> Set TypeConstraint -> Set TypeConstraint -> [TypeConstraint] -> Exc (Maybe (Set TypeConstraint))
_allNeededConstraints tt met seen wanted
	= do	newConstrs	<- wanted |+> neededConstraints tt met	:: Exc [Maybe [TypeConstraint]]
		let newConstrs'	= newConstrs & sequence |> concat |> L.filter (`S.notMember` seen)	:: Maybe [TypeConstraint]
		case newConstrs' of
			Nothing 	-> return Nothing
			(Just []) 	-> return $ Just seen
			(Just constrs)	-> _allNeededConstraints tt met (S.fromList constrs) constrs
