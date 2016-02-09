module Languate.TypeConstraint.Utils where

{--

This module implements helper functions to make life easier with typeconstraints, binding and such

--}

import StdDef
import Exceptions
import Languate.CheckUtils

import Languate.TAST
import Languate.Typetable.TypetableDef
import Languate.TypeConstraint.Def
import Languate.TypeConstraint.Bind

import Data.Set as S
import Data.List as L

import Control.Arrow
import Normalizable




asConstraints	:: RTypeReq -> Set TypeConstraint
asConstraints reqs
	= reqs |> first RFree & unmerge |> uncurry SubTypeConstr & S.fromList

-- constructs bindings (type requirements) from constraints for under bounded types (e.g. Bool is a 'b'); noBind is excluded
bindings	:: (Name -> Bool) -> TypeConstraint -> Maybe [(Name, RType)]
bindings noBind (SubTypeConstr a (RFree b))
 | noBind b
 		= Just []
 | otherwise	= Just [(b,a)]
bindings _ (SubTypeConstr _ _)
		= Just []
bindings nb (All constrs)
		= constrs |+> bindings nb |> concat
bindings nb (Choose a b)
		= do	abind	<- bindings nb a
			bbind	<- bindings nb b
			if abind == bbind then return abind
				else Nothing

{-
Constraints, as ["a0" is a X] are pretty useless if not a single a0 rests in the context.
Cleaning is done by expansion. Say ["a"] is in the used frees, thus each constraint which contains "a" is added.
We look what new frees are used in this constraints, which are recursively added

-}
removeUselessBinds	:: [Name] -> [TypeConstraint] -> [TypeConstraint]
removeUselessBinds usedFrees tcs
	= _addConstraints [] usedFrees (tcs |> (id &&& freesInConstraint))

{-
Used frees -> constraints -> (constraints to be added, unused constraints, *new* names)
-}
_addConstraints	:: [Name] -> [Name] -> [(TypeConstraint, [Name])] -> [TypeConstraint]
_addConstraints _ [] _
	= []
_addConstraints seen used constraints
	= let 	overlaps ls	= ls |> (`elem` used) & or
		(needed, unused)= constraints & L.partition (overlaps . snd)
		newUsed	= ((needed >>= snd) & nub) L.\\ (used++seen)
		recNeeded	= _addConstraints (seen++used) newUsed unused
		in
		(needed |> fst) ++ recNeeded

{-
Constructs the bindings from the constraints (except for certain frees)
e.g. buildBoundConstr ("_resultType" ==) [SubTypeConstr 'a' 'Int', SubTypeConstr 'Bool' 'b', SubTypeConstr '_resultType' 'a -> b']
== Just [Constraint a is pietervdvn:Data:Data.Nat.Nat,Constraint _resultType is (a -> pietervdvn:Data:Data.Bool.Bool)]
-}
buildBoundConstr	:: (Name -> Bool) -> [TypeConstraint] -> Exc (Maybe [TypeConstraint])
buildBoundConstr noBind constrs
	= inside ("In the binding of frees in "++show constrs) $
	  case bindings noBind $ All constrs of
		Nothing		-> return Nothing
		(Just binds)	-> All constrs & subsConstraint binds
					|> normalize |> flatten |> Just


isConstraintMet	:: Typetable -> TypeConstraint -> Exc Bool
isConstraintMet tt constr
	= isConstraintMet' tt S.empty constr


isConstraintMet'	:: Typetable -> Set TypeConstraint -> TypeConstraint -> Exc Bool
isConstraintMet' tt met constraint
	= do	allCons	<- allNeededConstraints tt met [constraint]
		case allCons of
			(Just set)	-> return $ S.null set
			_		-> return False
