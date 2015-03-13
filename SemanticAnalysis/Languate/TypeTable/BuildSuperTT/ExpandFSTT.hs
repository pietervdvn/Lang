module Languate.TypeTable.BuildSuperTT.ExpandFSTT where

{--
This module implements the FSTT expansion
--}

import StdDef hiding (todos)
import Exceptions

import Languate.CheckUtils

import Languate.TAST
import Languate.TypeTable
import Languate.TypeTable.Extended
import Languate.TypeTable.Bind.Substitute
import Languate.TypeTable.Bind.Bind

import Prelude hiding (null)
import Data.Set hiding (null, filter)
import qualified Data.Set as S

import Data.Map hiding (filter)
import qualified Data.Map as M
import qualified Data.List as L

import Data.Either (rights)
import Data.Tuple
import Data.Maybe

import Languate.Graphs.DirectedGraph

import State
import Control.Monad
import Control.Arrow

import Debug.Trace


buildSpareSuperTypeTable	:: FullSuperTypeTable -> SpareSuperTypeTable
buildSpareSuperTypeTable dict
	= dict & keys |> (\a -> (a,a)) ||>> getBaseTID
		|> swap |> unpackMaybeTuple & catMaybes	-- removing failed lookups
		& merge & M.fromList



{-
Makes the super type table complete, by recursively adding the supertypes of (known) super types.
-}
expand	:: (Map TypeID FullSuperTypeTable, [(RType, Set RType)]) ->
		Exc (Map TypeID FullSuperTypeTable, Map TypeID SpareSuperTypeTable)
expand (fstt, toCheck')
	= let 	initSstt	= fstt |> buildSpareSuperTypeTable
		initNotifTable	= initSstt	|> keys |> S.fromList & invertDict
		initTodo	= fstt 		|> keys |> S.fromList
		ctx	= Ctx fstt initNotifTable initSstt initTodo toCheck'
		ctx'	= runstate _expandAll ctx & snd
		-- stuff that has to be checked, as bind aways might have happened
		toBind	= toCheck ctx' in do
		checkSuperBinds (fstt_ ctx') (sstt_ ctx') toBind
		return $ ctx'  & (fstt_ &&& sstt_)

{-
During the calculation of the FSTT, values might have been bound where typer reqs are present.

e.g.
RSA is PubPrivAlgo RSAPrivKey RSAPubKey

This means that RSAPrivKey should be a PrivKey. This requirements are checked here
-}

checkSuperBinds	:: Map TypeID FullSuperTypeTable -> Map TypeID SpareSuperTypeTable -> [(RType, Set RType)] -> Check
checkSuperBinds fstt sstt toCheck
	= inside "While checking that implicit type requirements are met" $ do
		let toCheck'	= toCheck ||>> S.toList
		-- TODO PICKUP
		pass


type St	a = State Context a
data Context
	= Ctx {	fstt_	:: Map TypeID FullSuperTypeTable,
		  -- when t0 is changed --> notify these t1'ones
		notifyTable ::  DG TypeID,
		-- when t0 (in SSTT) has changed, you need to reimport all according RTypes
		sstt_	:: Map TypeID SpareSuperTypeTable,
		    	 -- recalculate t0, as rtypes (part of supertypes) have changed
		todos	:: Map TypeID (Set RType),
		-- the left type should be a subtype of all the right ones.
		-- These are requirements that should be met
		toCheck	:: [(RType, Set RType)] }




-- Do all the stuff! Main loop of the todos-list
_expandAll	:: St ()
_expandAll
	= do	isDone	<- done
		unless isDone $ do
		(tid, changed)	<- pop
		somethingChanged	<- mapM (_expand tid) changed |> or
		notifyChanged tid
		_expandAll

-- Adds the supertype of changedSuper to the FSTT of base
_expand	:: TypeID -> RType -> St Bool
_expand base changedSuper
     = 	-- any has no super types
	if base == anyTypeID then return False else
	-- changedSuper is some strange case -> we do not add it (this can happen with the initial settings to start the algo)
	if not $ isNormal changedSuper then return False else do
	-- convenience function to get the binding out of a FSSTentry
	let getBinding (_,_,tb)	= snd tb

	-- the fstt of the base type (which should get changed)
	fstt	<- get' fstt_ |> fetch (show base) "fstt_" base
	let via	= changedSuper
	let viaTid	= getBaseTID via & fromJust
	{- OldBinding converts the basetype into the via type.
		E.g. List (current) gets the type Mappable added via Dict k1 v1
		Then the oldbinding will convert the frees of the basetype of **Dict (the foreign type)**, namely (a0, a1)
		into the frees needed for this instance, thus {a0 --> k1, a1 --> v1}.

		This binding might produce a fixed binding, e.g. {a0 --> RSAPrivKey}
	 -}
	let oldBinding	= fetch (show via) "fstt" via fstt & getBinding
				:: Binding
	-- the supers FSTT, which we will tear apart and add to base
	supersToAdd	<- get' fstt_ |> findWithDefault M.empty viaTid |> M.toList
	mapM (_addEntry base via oldBinding) supersToAdd |> or


{-

Adds a supertypeentry to the basetype. The oldbinding takes the supertype(via) from its baseform to the needed form,

-}
_addEntry	:: TypeID -> RType -> Binding -> FullSTTKeyEntry -> St Bool
_addEntry base via oldBinding (superToAdd, (reqs, _, (_, newBinding)))
  = do	fstts		<- get' fstt_
	-- the fstt that has to be changed
	let fstt	= findWithDefault M.empty base fstts
	-- the new, combined binding ...
	let binding	= concatBindings newBinding oldBinding
	-- ... aplied on the super
	let super	= substitute binding superToAdd
	-- if already added: skip this shit
	if super `M.member` fstt then return False else do
	{- We calculate the requirements after substitution. These are the requirements we get from the foreign FSTT
		It is tested against the full binding -}
	foreignReqs	<- mapM (subReq binding) reqs |> catMaybes
	{- We might have some requirements on the via type too in our own FSTT-}
	let viaReqs	= fstt & M.lookup via |> (\(reqs, _, _) -> reqs) & fromMaybe []
	let newReqs	= (foreignReqs ++ viaReqs) & merge ||>> S.unions & filter (not . S.null . snd)
	let entry	= (newReqs, Just via, (superToAdd, binding))
	let fstt'	= M.insert super entry fstt
	let fstts'	= M.insert base fstt' fstts
	modify (\ctx -> ctx {fstt_ = fstts'})
	let mSuperTid	= getBaseTID superToAdd
	-- adds notifications. If mSuperTid does not exists -> superToAdd is a curry -> No notifications needed anyway
	when (isJust mSuperTid) $ notifyMe base (fromJust mSuperTid, superToAdd)
	return True


subReq	:: Binding -> (Name, Set RType) -> St (Maybe (Name, Set RType))
subReq binding@(Binding dict) (nm, reqs)
 | nm `M.member` dict
	= case M.lookup nm dict of
		-- Something got a new name: update requirements
		(Just (RFree nm'))
			-> return $ Just (nm', S.map (substitute binding) reqs)
		-- Something fused out of existance.
		-- e.g. a0 --> Bool, this means that we have to check bool against all requirements -> we do binding afterwards, when the entire table has been built
		(Just typ)	-> do	addReqs (typ, reqs)
					return Nothing

 | otherwise	= return $ Just (nm, S.map (substitute binding) reqs)
-----------
-- UTILS --
-----------

-- Wether we are done or not
done	:: St Bool
done	= get' todos |> M.null

-- Gives the next type id to update + what of its super (current) super types should be re added
pop	:: St (TypeID, [RType])
pop	= do	td	<- get' todos
		let all@(nxt, changed)	= M.findMin td |> S.toList
		modify (\ctx -> ctx {todos = M.delete nxt td})
		return all

addReqs		:: (RType, Set RType) -> St ()
addReqs reqs
	= do	ctx	<- get
		put $ ctx {toCheck = reqs : toCheck ctx}

notifyChanged	:: TypeID -> St ()
notifyChanged tid
	= do	toNotifs	<- get' notifyTable |> nodesFrom tid |> S.toList
		mapM_ (notifyChanged' tid) toNotifs


notifyChanged'	:: TypeID -> TypeID -> St ()
notifyChanged' changed toNotif
	= do	-- get the SpareSuperTypeTable of the type that has to be notified
		sstt	<- get' sstt_ |> fetch (show toNotif) "sstt_" toNotif
		-- now get the actual types that might have changed
		let supers	= fetch (show changed) ("sstt for "++show toNotif)
					changed sstt & S.fromList
		todos'	<- get' todos |> insertWith S.union toNotif supers
		modify (\ctx -> ctx {todos = todos'})



notifyMe	:: TypeID -> (TypeID, RType) -> St ()
notifyMe tidToNotif (changedTid, because)
	= do	-- update notifytable
		notifT	<- get' notifyTable |> addLink (changedTid, tidToNotif)
		modify (\ctx -> ctx {notifyTable = notifT})
		-- update sstt
		fsstt	<- get' sstt_
		let sstt	= findWithDefault M.empty tidToNotif fsstt
		let baseSet	= findWithDefault [] changedTid sstt
		unless (because `elem` baseSet) $ do	-- because is not added yet
		let sstt'	= M.insert changedTid (because: baseSet) sstt
		let fsstt'	= M.insert tidToNotif sstt' fsstt
		modify (\ctx -> ctx {sstt_ = fsstt'})



fetch	:: (Ord n) => String -> String -> n -> Map n k -> k
fetch str dict
	= findWithDefault (error $ "Key "++str++" not found in dict "++show dict)
