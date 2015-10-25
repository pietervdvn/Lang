module Languate.Typetable.PropagateSupertypes where

{--
This module calculates the 'transitive closure' of the supertypes, and adds them to the ti's
--}

import StdDef
import Exceptions
import Languate.CheckUtils

import Graphs.DirectedGraph
import Graphs.ExportCalculator
import Data.Map as M

import Languate.TAST
import Languate.Typetable.TypetableDef

type Changed	= Bool

-- adds all supertypes, also a few steps away
propagateSupertypes		:: Typetable -> Exc Typetable
propagateSupertypes tt
	= do	-- we could optimize this code with an epxort graph, but... let's do it the naive way :p
		whileChanged propagSupertypesStep tt


propagSupertypesStep	:: Typetable -> Exc (Typetable, Changed)
propagSupertypesStep tt@(Typetable conts)
	= do	conts'	<- conts & M.toList |+> uncurry (addSupersFor tt) |> M.fromList |> Typetable)
		return (Typetable conts', conts /= conts')



addSupersFor	:: Typetable -> TypeID -> TypeInfo -> Exc (TypeID, TypeInfo)
addSupersFor tt tid ti
	= inside ("While calculating all the supertypes for "++show tid) $
	  do	return (tid, ti)


supertypesFor	:: Typetable -> (RType, [TypeConstraint]) -> Exc [(RType, [TypeConstraint])]
supertypesFor tt (superform, constraints)
	= inside ("While getting the supertypes of "++show superform) $
	  do	-- let's get the TI of this supertype
		superTID	<- getBaseTID superform ? ("No tid for "++show superform)
		return []
