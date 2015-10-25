module Languate.Typetable.PropagateImplicitConstraints (typeRequirementsOn, propagateImplicitConstraints) where
{- Propagates constraints on the super types, e.g. instance List is Bag, the implicit requirement is that (a0:Eq).

This module defines all functions to propagate constraints on the existance of types (thus constraints imposed by inhereting other types).

It does not deal with 'instance X is Y if a0 is Z'
  -}

import StdDef
import Exceptions
import Languate.CheckUtils
import HumanUtils


import Graphs.SearchCycles
import Graphs.Order

import Languate.Typetable.TypetableDef
import Languate.Typetable.TypeLookupTable
import Languate.Typetable.ModuleTraverser
import Languate.TAST
import Languate.AST hiding (frees)
import Data.Set as S
import Data.Map as M
import Data.List as L
import Data.Maybe
import Data.Either
import Data.Tuple
import Control.Arrow
import Control.Monad


type Changed	= Bool

propagateImplicitConstraints tlt mod tt
	= (`whileChanged` tt)
		(\tt_ -> do	-- first we propagate the implicit type requirements on cat-declarations
				-- as long as we are changing: propagete the implicit type parameters; this might not be complete
				(tt', changed')		<- whileChanged (propagateParams tlt mod) tt_
				-- now the implicit requirements on requirement: X a (b:Set a) => a:Eq
				(tt'', changed'')	<- whileChanged propagateMetaRequirements tt'
				return (tt'', changed' || changed''))



{-
Propagates requirements which are known via other requirements, e.g:

type X a (b:Set a)
implies that a:Eq, as Set (a:Eq)

this concerns all TYPE DECLARATIONS
-}
propagateMetaRequirements	:: Typetable -> Exc (Typetable, Changed)
propagateMetaRequirements tt@(Typetable conts)
	=  do	conts'	<- conts & M.toList |+> (\(tid, ti) -> do	ti'	<- propagateMetaRequirementsIn tt tid ti
									return (tid, ti'))
					:: Exc [(TypeID, (TypeInfo, Changed))]
		let (conts'', changed)	= conts' |> (\(tid, (ti, changed)) -> ((tid, ti),changed)) & unzip |> or
		return (Typetable $ M.fromList conts'', changed)

-- we take a look at the constraints, and explore these further
propagateMetaRequirementsIn	:: Typetable -> TypeID -> TypeInfo -> Exc (TypeInfo, Changed)
propagateMetaRequirementsIn tt tid ti
	= inside ("While calculating implicit type requirements on "++show tid) $
	  do	let constraintTypes	= constraints ti & M.elems & concat	:: [RType]
		ti' 		<- foldM (propagateMetaRequirementsIn' tt tid) ti constraintTypes
		return (ti', ti /= ti')


propagateMetaRequirementsIn'	:: Typetable -> TypeID -> TypeInfo -> RType -> Exc TypeInfo
propagateMetaRequirementsIn' tt@(Typetable conts) tid ti reqT
	= do	let nrOfFrees	= ti & frees & length
		-- {0 --> a0, 1 --> a1, ... }
		let defaultMap	= zip [0..] defaultFreeNames & take nrOfFrees
		reqTi		<- getTi tt reqT
		let args	= appliedTypes reqT
		let this2req	= zip defaultFreeNames args
		let reqConstr	= constraints reqTi & M.toList
					:: [(Int, [RType])]
		-- constraint types, with frees expressed in as frees of this declaration (and not the foreign one)
		reqConstr'	<- reqConstr |+> onSecond (|+> subs this2req)

		{-  {argument --> should be} -}
		let newConstr	= buildMapping (zip args [0..]) reqConstr'
		return $ addConstrReqs' defaultMap newConstr ti




{-
Propagates the requirements on the type variables.
e.g.
cat A (a:X)
cat B a:A a

In the second cat, it is implied that B (a:Eq)
For CATEGORY-DECLARATIONS and TYPE-DECLARATIONS only, as the constraints imposed here are constraints to exist, not to have a certain supertype
type a = {...} + ADOPTS
-}
propagateParams	:: TypeLookupTable -> Module -> Typetable -> Exc (Typetable, Changed)
propagateParams tlt mod tt@(Typetable dict)
	= do	let depGraph	= dict |> supertypes |> M.keys ||>> getBaseTID |> catMaybes |> S.fromList  :: Map TypeID (Set TypeID)
		order	<- buildOrdering depGraph & either (\e -> halt $ "Cycles in the supertypes: "++show e) (return . reverse . fst)

		adoptions	<- mod & statements |+> constraintAdoptions tlt |> concat |> merge	:: Exc [(RType, [RType])]
		let adoptions'	= adoptions |> (fst &&& id) |> first getBaseTID |> unpackMaybeTuple & catMaybes :: [(TypeID, (RType, [RType]))]
		foldM (\(tt@(Typetable content), changed) typeToInv ->
			do	ti	<- M.lookup typeToInv content ? ("No type info found for "++show typeToInv++", weird")
				(ti', changed')	<- propagateParamConstraintsOfTID tt adoptions' typeToInv ti
				return (Typetable $ M.insert typeToInv ti' content, changed || changed') ) (tt, False) order

-- looks for super types of 'tid' into the 'supers'-list, propagates constraints on those withint ti
propagateParamConstraintsOfTID	:: Typetable -> [(TypeID, (RType, [RType]))] -> TypeID -> TypeInfo -> Exc (TypeInfo, Changed)
propagateParamConstraintsOfTID tt supers tid ti
	= inside ("While propagating the implicit type parameters for the type "++show tid) $
	  L.lookup tid supers & maybe (return (ti, False))
		(\(actualForm, supers')	-> foldM (\(ti, changed) super ->
						propagateParamConstraintsIn tt (actualForm, super) ti
						||>> (|| changed)) (ti, False) supers')


{- fixes a single typeInfo, by adding constraints to the type (basetype of the first in the tuple). This might be incomplete, as dependencies might not know their requirements yet; this is fixed by multiple successive invocations (see buildTable) 	-}
propagateParamConstraintsIn	:: Typetable -> (RType, RType) -> TypeInfo -> Exc (TypeInfo, Changed)
propagateParamConstraintsIn tt@(Typetable dict) (actualForm', superForm') ti
	= inside ("While propagating the implicit type parameters from the supertype "++show superForm') $
	  do	{-----------------------}
		{- normalize the frees -}
		{-----------------------}
		-- subMapping {a --> sub0, b --> sub1, ...}
		let (actualForm, subMapping')	= normalizeRT "sub" actualForm' ||>> swap
		-- superMapping : {actualName --> normalized supername}; e.g. {b --> super0, c --> super1}
		let (superForm_, superMapping)	= normalizeRT "super" superForm'

		-- {Free type variable index --> normalized names}
		let subNames			= zip [0..] $ defaultFreeNames' "sub" & take (length subMapping')	:: [(Int, Name)]
		-- {Free type variable index --> normalized name, e.g. sub5}
		let subMapping			= buildMapping subNames subMapping'
		-- maping: {normalized super name -> normalized sub name} {super0 --> sub1, super1 --> sub2}
		let mapping 		= buildMapping subMapping' superMapping |> swap
		-- normalized type, expressed in actual types or 'sub0'-type variables
		superForm		<- subs (mapping ||>> RFree) superForm_
		{--------------------------------------}
		{- Calculate and add new requirements -}
		{--------------------------------------}
		ti'	<- propagateImplicitRequirements tt subNames superForm ti
		let changed	= ti /= ti'
		return (ti', ti /= ti')








{- propagates requirement on direct supertypes.
	e.g. cat A a b:Set a implies a:Eq, this function add those hidden requirements -}
propagateImplicitRequirements	:: Typetable -> [(Int, Name)] -> RType -> TypeInfo -> Exc TypeInfo
propagateImplicitRequirements tt mapping rt ti
	= do	requirements	<- typeRequirementsOn tt rt
		let mapping	= zip [0..] (defaultFreeNames' "sub") & take (length $ frees ti)
		return $ addConstrReqs' mapping requirements  ti


{- Gets the requirements on a type, recursively, independent of what to add it to	-}
typeRequirementsOn	:: Typetable -> RType -> Exc [(RType, [RType])]
typeRequirementsOn tt superForm
	= inside ("While fetching the implicit type requirements on "++show superForm) $
	  do	ti		<- getTi tt superForm
		let knd		= kind ti
		let args	= appliedTypes superForm
		assert (length args == numberOfKindArgs knd)
			("The type "++ show superForm ++" is applied to too little (or too much) arguments.")
		-- free type variables are expressed in foreign type
		let constr'	= constraints ti & M.toList
		-- mapping
		let form2foreign	= zip args defaultFreeNames
						& L.filter (isRFree . fst)
						|> first (\(RFree nm) -> nm) :: [(Name, Name)]
		let form2foreign'	= form2foreign |> second RFree
		let foreign2form	= form2foreign |> swap
		let foreign2form'	= foreign2form |> second RFree
		-- constraint with free type variables expressed in actual values
		constr		<- constr' |+> onSecond (|+> subs form2foreign')
		let subIsA	= zip args [0..] & flip buildMapping constr	:: [(RType, [RType])]
		-- we undo the translation, for the recursive call. This means the requirements are again in a0 form
		subIsA'		<- subIsA |+> onSecond (|+> subs foreign2form')
		recConstraints		<- (subIsA' >>= snd ) |+> typeRequirementsOn' tt |> concat	:: Exc [(RType, [RType])]
		return (subIsA++recConstraints)

typeRequirementsOn'	:: Typetable -> RType -> Exc [(RType,[RType])]
typeRequirementsOn' _ (RFree _)
			= return []
typeRequirementsOn' _ (RNormal{})
			= return []
typeRequirementsOn' tt (RCurry t1 t2)
			= do	t1Reqs	<- typeRequirementsOn' tt t1
				t2Reqs	<- typeRequirementsOn' tt t2
				return $ t1Reqs ++ t2Reqs
typeRequirementsOn' tt appliedType
			= typeRequirementsOn tt appliedType




-- utility; add all at once
addConstrReqs'	:: [(Int, Name)] -> [(RType, [RType])] -> TypeInfo -> TypeInfo
addConstrReqs' mapping requirements
	= let	(constr, reqs)	= L.partition (isRFree . fst) requirements
		constr'		= constr |> first (\(RFree nm) -> nm) in
		addConstrReqs mapping (constr', unmerge reqs)



addConstrReqs	:: [(Int, Name)] -> ([(Name, [RType])], [(RType, RType)])
			-> TypeInfo -> TypeInfo
addConstrReqs mapping (constraints, reqs) ti
	= ti & addConstraintsWith mapping constraints & addRequirements reqs
