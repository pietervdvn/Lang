module Languate.TypeConstraint.Utils where

import StdDef
import Languate.CheckUtils

import Languate.TAST
import Languate.Typetable.TypetableDef
import Languate.TypeConstraint.Def

{-
Binds a type in another type.
E.g.
A -> B `bind` a -> b
is only possible if "a == A", "b == B"
This gives a lot of type constraints, which might be solved.
Note that these have to be solved even further to see if it is actually possible
-}
bnd	:: RType -> RType -> TypeConstraint
bnd t0@(RApplied base0 arg0) t1@(RApplied base1 arg1)
	=  choose t0 t1
		[bind base0 base1, bind arg0 arg1]
bnd t0@(RCurry base0 arg0) t1@(RCurry base1 arg1)
	= choose t0 t1
		[bind base0 base1, bind arg0 arg1]
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
- free types
-}
isSuper	:: Typetable -> RType -> RType -> Maybe TypeConstraint
isSuper tt t0 t1
 | t0 == t1	= Just $ All []
 | otherwise	= _isSuper tt t0 t1


_isSuper	:: Typetable -> RType -> RType -> Maybe TypeConstraint
_isSuper tt t0 t1
	= todo
















{-
Given a type constraints, gives the constraints that are needed to meet this constraints.
Returns nothing if this constraint can never be met in the current type table
-}
neededConstraints	:: Typetable -> TypeConstraint -> Maybe [TypeConstraint]
neededConstraints tt constr
	= neededConstraints' tt constr

neededConstraints'	:: Typetable -> TypeConstraint -> Maybe [TypeConstraint]
neededConstraints' tt (SubTypeConstr sub super)
	= todo
