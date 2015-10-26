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
import Data.List as L

import Languate.TAST
import Languate.Typetable.TypetableDef

type Changed	= Bool

-- adds all supertypes, also a few steps away
propagateSupertypes		:: Typetable -> Exc Typetable
propagateSupertypes tt
	= do	-- we could optimize this code with an epxort graph, but... let's do it the naive way :p
		whileChanged propagSupertypesStep tt |> fst


propagSupertypesStep	:: Typetable -> Exc (Typetable, Changed)
propagSupertypesStep tt@(Typetable conts)
	= do	conts'	<- conts & M.toList |+> uncurry (addSupersFor tt) |> M.fromList
		return (Typetable conts', conts /= conts')



addSupersFor	:: Typetable -> TypeID -> TypeInfo -> Exc (TypeID, TypeInfo)
addSupersFor tt tid ti
	= inside ("While calculating all the supertypes for "++show tid) $
	  do	supertypes'	<- ti & supertypes & M.toList |+> supertypesFor tt ti |> concat	:: Exc [(RType, [TypeConstraint])]
		return (tid, ti {supertypes = M.fromList supertypes'})

-- calculates the new supertypes
supertypesFor	:: Typetable -> TypeInfo -> (RType, [TypeConstraint]) -> Exc [(RType, [TypeConstraint])]
supertypesFor tt ti orig@(superform, constraints)
	= inside ("While getting the supertypes of "++show superform) $
	  do	supers		<- supertypesOf tt superform
		let gotAlready	= supertypes ti & M.keys
		let supers'	= supers & L.filter ((`notElem` gotAlready) . fst)
		warn $ "Available supers " ++ show supers'
		let newSupers	= supers' ||>> (constraints ++ ) ||>> nub
		return (orig:newSupers)
