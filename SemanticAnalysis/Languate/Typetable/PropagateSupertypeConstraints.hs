module Languate.Typetable.PropagateSupertypeConstraints where

{--
This module propagates constraints on super type declarations.
Some super types have implicit constraints on them, and are added via functions here.
--}

import StdDef
import Exceptions
import Languate.CheckUtils

import Languate.TAST
import Languate.AST
import Languate.Typetable.TypetableDef
import Languate.Typetable.TypeLookupTable
import Languate.Typetable.PropagateImplicitConstraints (typeRequirementsOn)
import Languate.Typetable.ModuleTraverser


import Data.List as L
import Data.Map as M
import Data.List (nub)
import Control.Monad

addSuperConstraints	:: TypeLookupTable -> TypeSTMs -> Typetable -> Exc Typetable
addSuperConstraints tlt stms tt@(Typetable conts)
	=  do	-- instance X is Y supertype constraint propagation, the normal case
		tt'	<- conts & M.toList |+>  uncurry (addSuperConstraintsFor tt)
				|> M.fromList |> Typetable
		-- pushed upon supertypes
		pushedUpons	<- stms |+> pushedSupers tlt |> concat	:: Exc [(RType, (RType, [Name]))]
		foldM addPushedUpon tt' pushedUpons


addPushedUpon	:: Typetable -> (RType, (RType,[Name])) -> Exc Typetable
addPushedUpon (Typetable conts) (subForm, (super, frees))
	= inside ("While adding constraints to "++show subForm++" imposed by "++show super) $
	  do	-- let's get the TID and TI of the subform, the table we have to change
		subTid		<- getBaseTID subForm ? ("No tid found for "++show subForm++" (propagateSupertypeConstraints)")
		subTi		<- conts & M.lookup subTid ? ("No type info found for "++show subTid++", weird")
		-- we DON'T need constraints of the super type!
		-- playing with the free type variables
		let mapping'	= zip frees defaultFreeNames
		let mapping	= mapping' ||>> RFree
		-- apply the frees to the superform
		let superForm'	= applyTypeArgs super (mapping |> snd)
		-- express subform in terms of the default free names
		subForm'	<- subs mapping subForm
		-- get applied arguments of the subForm, number them...
		-- e.g. ''List Char is String'' => subArgs will be {a0 --> Char}
		let subArgs	= appliedTypes subForm' & zip (defaultFreeNames |> RFree)
		-- these are our constraints!
		let subConstraints	= subArgs |> uncurry SubTypeConstr
		-- actual update, boilerplate
		let superTps'	= M.update (\oldConstr -> Just $ nub $ L.filter (not . isTrivialConstraint) (oldConstr ++ subConstraints))
					superForm' (supertypes subTi)
		let ti'		= subTi {supertypes = superTps'}
		let conts'	= conts & M.insert subTid ti'
		return $ Typetable conts'


{- Check super type constraints, by taking each super type and comparing it by the actual constraints -}
addSuperConstraintsFor	:: Typetable -> TypeID -> TypeInfo ->
		Exc (TypeID, TypeInfo)
addSuperConstraintsFor tt tid ti
	= inside ("While adding the supertype constraints of the type "++show tid) $
	  do	supertypes'	<- ti & supertypes & M.toList |+> fetchConstraints tt |> M.fromList
		return (tid, ti {supertypes = supertypes'})



fetchConstraints	:: Typetable -> (RType, [TypeConstraint]) ->
				Exc (RType, [TypeConstraint])
fetchConstraints tt (rt, constraints)
	= inside ("While adding the constraints for supertype "++show rt) $
	  do	constr	<- typeRequirementsOn tt rt
				|> unmerge ||>> uncurry SubTypeConstr
		return (rt, L.filter (not . isTrivialConstraint) $ nub (constr++constraints))
