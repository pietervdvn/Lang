module Languate.TypeTable.Bind.Bind where

{--
This module implements a simple bind, which works on Full Super Type Tables
--}

import StdDef hiding (isRight)
import StateT

import Graphs.DirectedGraph
import HumanUtils (isAre)

import Data.Map hiding (foldr)
import qualified Data.Map as M
import Data.Set hiding (foldr)
import qualified Data.Set as S
import Data.List (intercalate)
import qualified Data.List as L

import Data.Tuple
import Data.Maybe
import Data.Either
import Prelude hiding (fail, lookup, catch)


import Languate.TAST
import Languate.TypeTable as TT
import Languate.TypeTable.Bind.Substitute
import Languate.TypeTable.Bind.StMsg as StMsg


import Control.Monad hiding (fail)
import Control.Monad.Trans
import Control.Arrow


import Debug.Trace
import Data.Function hiding ((&))

{-

Binds t0 in t1. If binding succeeds, this means t0 is a subtype (or equal type) of t1.

For each applied type, at most one supertypetable convert can happen.

Binding via requirements can happen too.
When free ''a'' is bound against free ''b'', each requirement on ''a'' is matched on each requirement on ''b''.

Assumes that t0 has no overlapping free names with t1.
-}
_bind'	:: Bool -> Map TypeID SpareSuperTypeTable -> Map TypeID FullSuperTypeTable -> KindLookupTable -> Map Name [RType] -> RType -> RType -> Either String Binding
_bind' recursive sstt fstt klt reqs t0 t1
	= let 	used	= (t0:t1:concat (M.elems reqs)) |> freesInRT & concat & S.fromList
		ctx	= Context reqs used sstt fstt noBinding klt recursive
		msg	= "While binding "++show t0++" in "++ show t1 in
		runstateT (inside msg $ bind' t0 t1) ctx |> snd |> binding_

_bind	= _bind' True

bind	:: TypeTable -> Map Name [RType] -> RType -> RType -> Either String Binding
bind tt	= _bind (TT.spareSuperTypes tt) (TT.allSupertypes tt) (TT.kinds tt)


isSubtype	:: TypeTable -> Map Name [RType] -> RType -> RType -> Bool
isSubtype tt reqs t0 t1
	= bind tt reqs t0 t1 & isRight

cleanBind	:: Bool -> RType -> RType -> StMsg (Either String Binding)
cleanBind recursive t0 t1
	= do	ctx	<- get
		return $ _bind' recursive (StMsg.spareSuperTypes ctx) (StMsg.allSupertypes ctx) (StMsg.kinds ctx)  M.empty t0 t1


bind'	:: RType -> RType -> StMsg ()
bind' (RFree a) (RFree b)
 = do	aReqs	<- requirementsOn a
	bReqs	<- requirementsOn b
	addBinding (b, RFree a)
	-- bind each aReq against each bReq. Each bReq should be filled in with at least one aReq
	{- succ	<- mapM (\b -> mapM (\a -> succeeded $ bind' a b) aReqs) bReqs
	-- internal list: bind each aReq against a bReq. One match is enough
	-- extarnal list: each bReq is tried. All should have a match
	let ok	= succ |> or & and
	assert ok $ "Could not bind the free '"++a++"' against the free '"++
		b++"' as '"++b++"' has some requirements that '"++a++"' can't meet." ++
		indent ("\n'"++a++"': "++show aReqs++"\n'"++b++"': "++show bReqs)
		TODO can this check really be omittded?	-}
bind' t0 (RFree b)
 = do	bReqs	<- requirementsOn b
	addBinding (b, t0)
	inside ("While binding "++show t0++" against the type requirements on '"++b++"'") $
		mapM_ (bind' t0) bReqs
bind' (RFree a) t1
 = do	-- if the free has the right super type, we can assume binding is OK
	aReqs	<- requirementsOn a
	ok	<- mapM (\sub -> succeeded $ bind' sub t1) aReqs |> or
	return ()
	{- assert ok $ "Could not bind the free '"++a++"'"++
			" as it does not have the necessary super type "++st True t1++
			".\nIt has the supertypes "++show aReqs
			TODO can this check really be ommited?-}
bind' (RCurry t0 t1) (RCurry t0' t1')
 = do	bind' t0 t0'
	bind' t1 t1'
bind' (RCurry _ _) _
 = fail "Curry-types  (a->b) can not have super types"
bind' t0@(RNormal fqn nm) t1
 = when (t0 /= t1) $	-- if they are the same, nothing should happen
	-- we search if t1 is a supertype of t0
	superBind t0 t1
bind' t0@(RApplied bt at) t1@(RApplied bt' at')
 = try' (superBind t0 t1) $ \msg ->
	-- first try binding via the supertype, if not just recursive
	inside ("In the binding of "++show t0++" in "++show t1++indentl msg++"\n") $ do
	bind' bt bt'
	bind' at at'
bind' t0@(RApplied bt at) t1
 	= superBind t0 t1

-- tries to bind t0 into t1 via a super type table lookup
superBind	:: RType -> RType -> StMsg ()
superBind sub wantedSuper
	= inside ("Trying to bind "++st True sub++" into "++st True wantedSuper) $
	  try' (lookupSuperFrees sub wantedSuper) $ \msg -> -- first, try to bind via superfrees. If that fails, attempt conventional supertype lookup
	  do	-- we calculate all possible super forms, which we try against match superBind
		subTid	<- getBaseTID sub ? ("No tid for "++show sub)
		availableForms	<- wantedFormsOf sub subTid wantedSuper
		let isLeft	= either (const True) (const False)
		failed	<- whileM' isLeft (\availableForm -> catch' $
				lookupSupersAgainst sub availableForm wantedSuper) availableForms
		let failedMsg tp (Left reason)
				= st True tp ++ " failed as "++indentl reason
		assert (length failed /= length availableForms) $ msg++"\n"++
			"Could not bind "++st True sub++" in "++st True wantedSuper++" as no form can be matched."++
				indentl ("Tried types: "++indentl (zip availableForms failed |> uncurry failedMsg & unlines))

-- gives available types, thus the types that the subType has as supertype which contain (more or less) the wanted super
wantedFormsOf	:: RType -> TypeID -> RType -> StMsg [RType]
wantedFormsOf subType subTid wantedSuper
 | isNormal wantedSuper
	= do	supTid	<- getBaseTID wantedSuper ?
				("Huh? wanted super "++show wantedSuper++" is normal!")
		-- normal super types available from the supertypetables
		getSstt subTid |> findWithDefault [] supTid
 | otherwise
	= getFstt subTid |> keys |> L.filter (not . isNormal)

{-

Constructions as "Encrypted a is a" are possible.

This method binds those.

"Encrypted (Encrypted Message)" "Message"

-}
lookupSuperFrees	:: RType -> RType -> StMsg ()
lookupSuperFrees subT superT	= do
		subTid		<- getBaseTID subT ? "Huh? subT should be normal!"
		-- we get the frees which act as supertype. If one of the super types is a free, it might be the super type we need!
		freeSupers	<- getFstt subTid |> M.keys |> Prelude.filter isRFree ||>> (\(RFree nm) -> nm)
		-- lets build the substitution scheme! We use "bind" for that :p
		klt	<- get' StMsg.kinds
		cleanB	<- cleanBind False subT (vanillaType' klt subTid)
		rec	<- get' recursive
		let extract (Binding d)	= freeSupers |> (`M.lookup` d) & catMaybes	:: [RType]
		let extracted	= case cleanB of
					Left _	-> []
					Right b	-> extract b
		let recursiveSupers	= if rec then extracted else []	:: [RType]
		let msg	= "Could not bind "++st True subT++" in "++st True superT++" recursively"
		assert (not $ Prelude.null recursiveSupers) $ msg ++ " as it has no free super types."
		-- actual binding
		let isLeft	= either (const True) (const False)
		failed	<- whileM' isLeft (\availableForm -> catch' $ bind' availableForm superT) recursiveSupers
		assert (length failed /= length recursiveSupers) $
			msg++" as no form can be matched.\nTried supers: "++show (zip recursiveSupers failed)

{-
Given a subtype, a form of a super type (from the FSTT) and the wanted super type, tries to perform binding (or fails)
-}
lookupSupersAgainst	:: RType -> RType -> RType -> StMsg ()
lookupSupersAgainst t availableForm wantedType
 | isNormal t
   = inside ("Looking up supers of "++st True t++", these are: "++show availableForm++", wanted super: "++show wantedType) $ do
	tidSub		<- getBaseTID t ? "Huh? T0 should be normal!"
	stt		<- getFstt tidSub
	-- gives the applied types, which should get bound against the requirements
	-- substitution gives ""availableForm --> t""
	let baseBinding	= appliedTypes t & zip defaultFreeNames
	{- requirements on the frees.
		These bindings might be important, to fill in the super type
		""availableForm-frees --> requirements""-}
	freeReqs	<- lookup availableForm stt |> reqs  ?
				(show t ++ " does not have a supertype "++show availableForm)
	assert (length freeReqs >= length baseBinding) $ "The type "++show t++
		" has been applied to too many arguments, only "++
		show (length freeReqs)++" are needed"
	assert (length freeReqs <= length baseBinding) $ "The type "++show t++
		" has been applied to too little arguments, "++
		show (length freeReqs)++" "++isAre (length freeReqs)++ " needed (or "++show wantedType++" has been applied to too many arguments)"
	-- the binding which converts the the wanted type into available form
	-- wantedType --> availableForm
	form2type	<- isolate $ bind' availableForm wantedType >> getBinding
	-- gives the full binding in the opposite direction
	let fullBind	= chainBindings form2type (baseBinding & M.fromList & Binding) & unbind & M.toList	:: [(Name,RType)]
	mapM_ addBinding fullBind




 | otherwise	= fail $ "The type "++show t++" is not normal"




-- Binds each sub in each super of matching names
bindSameAgainst	:: [(Name, RType)] -> Map Name RType -> StMsg ()
bindSameAgainst subs supers
	= mapM_ (\(nm, sub) -> bind' sub $ M.findWithDefault sub nm supers) subs

{- Binds the found applied types against their requirements (fetched via the supertypetable)
 Returns (the bindaway to resolve conflicts, e.g. "k1" --> "free_k1" (and its reverese) and the binding of some frees which got bound by recursive calls -}
_fixRequirements	:: [(Name, Set RType)] -> [(Name, RType)] -> StMsg (Map Name Name, Binding)
_fixRequirements reqs typeArgs
	-- reqs	= ""availableForm-frees --> requirements""
	-- typeArgs	= ""availableForm --> what we got"", e.g. a0
	= do	-- first, we calculate what free type variables are used in the supertype
		-- we bind those away as to prevent weird infections
		frees		<- getUsedFrees |> S.toList
		let bindAway	= buildBinding' frees
		-- lets add all those frees!
		-- addFrees $ M.elems bindAway
		let bindAway'	= asBinding bindAway
		-- we replace all "a0","a1",... in the known types.
		let fetchName oldName	= findWithDefault oldName oldName bindAway
		let reqs'	= reqs 	|> first fetchName
					|> second (S.map $ substitute bindAway')
		-- type args have the form "a0" of the applied type is "type in context".
		-- only the keys(!) have to be translated!
		let typeArgs'	= typeArgs |> first fetchName ||>> S.singleton
		-- now, lets bind the requirements!
		-- let bindAllReqs	= mapM_ (\(nm, bt) -> bindAll bt (L.lookup nm reqs' |> S.toList & fromMaybe [])) typeArgs'
		reqBound	<- isolate $ mergedReqBinding (M.fromList typeArgs') (M.fromList reqs) >> getBinding
		return (bindAway, reqBound)

{-
Binds based on name.
Binding0 (subs)		= {a0 --> x, a1 --> Nat, b --> Eq}
Binding1 (supers)	= {a0 --> y, a1 --> Eq}
will result in a binding from subs to supers, where the original name is the same:

{x --> y}
Nat `subtypeOf` Eq (so that's ok)
b has no counterpart (so that's ok)
-}
mergedReqBinding	:: Map Name (Set RType) -> Map Name (Set RType) -> StMsg ()
mergedReqBinding subs supers
 | M.null supers	= return ()	-- all supertypes have been bound, we're done
 | otherwise	= do	let (nm, superTypes)	= M.findMax supers |> S.toList
			let subTypes	= M.findWithDefault S.empty nm subs & S.toList
			-- we try binding each subtype into each supertype. Each supertype should have at least one subtype that binds into it.
			ok	<- superTypes & mapM (\super ->
					subTypes & mapM (\sub -> bind' sub super & succeeded) |> or) |> and
			assert ok $ nm++" has some unmet requirements. It should be: "++indentl(superTypes |> st True & unlines)++"but the only available types are:"++
					indentl (subTypes |> st True & unlines)
			mergedReqBinding subs (supers & M.delete nm)


-- ## TOOLS


nameFor	:: [Name] -> Name -> Name
nameFor used free
 | ("free_"++free) `L.notElem` used	= "free_"++free
 | otherwise	= let 	ind	= while (\ind -> nameFor' free ind `elem` used) (+1) 0 in
			nameFor' free ind


nameFor'	:: Name -> Int -> Name
nameFor' free ind
		=  "free_"++show ind++free


-- binds the subtype into all supertypes. Fails if one type fails
bindAll	:: RType -> [RType] -> StMsg ()
bindAll sub
	= mapM_ (bind' sub)
