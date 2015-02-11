module Languate.TypeTable.ExpandFSTT where

{--
This module implements the FSTT expansion
--}

import StdDef hiding (todos)

import Languate.TAST
import Languate.TypeTable
import Languate.TypeTable.Extended
import Languate.TypeTable.Bind.Substitute

import Prelude hiding (null)
import Data.Set as S hiding (null, filter)
import Data.Map hiding (filter)
import qualified Data.Map as M
import qualified Data.List as L

import Data.Either (rights)
import Data.Tuple
import Data.Maybe

import Languate.TypeTable.Bind.Substitute

import Languate.Graphs.DirectedGraph

import State
import Control.Monad

import Debug.Trace


buildSpareSuperTypeTable	:: FullSuperTypeTable -> SpareSuperTypeTable
buildSpareSuperTypeTable dict
	= dict & keys |> (\a -> (a,a)) ||>> getBaseTID
		|> swap |> unpackMaybeTuple & catMaybes	-- removing failed lookups
		& merge & M.fromList



{-
Makes the super type table complete, by recursively adding the supertypes of (known) super types.
-}
expand	:: Map TypeID FullSuperTypeTable ->
		(Map TypeID FullSuperTypeTable, Map TypeID SpareSuperTypeTable)
expand fstt
	= let 	initSstt	= fstt |> buildSpareSuperTypeTable
		initNotifTable	= initSstt	|> keys |> S.fromList & invertDict
		initTodo	= fstt 		|> keys |> S.fromList
		ctx	= Ctx fstt initNotifTable initSstt initTodo	in
		runstate _expandAll ctx & snd & (\ctx -> (fstt_ ctx, sstt_ ctx))


type St	a = State Context a
data Context
	= Ctx {	fstt_	:: Map TypeID FullSuperTypeTable,
		  -- when t0 is changed --> notify these t1'ones
		notifyTable ::  DG TypeID,
		-- when t0 (in SSTT) has changed, you need to reimport all according RTypes
		sstt_	:: Map TypeID SpareSuperTypeTable,
		    	 -- recalculate t0, as rtypes (part of supertypes) have changed
		todos	:: Map TypeID (Set  RType) }




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
	= do	if (base == anyTypeID) then return False else do
		fstt	<- get' fstt_ |> fetch (show base) "fstt_" base
		let via	= changedSuper
		let viaTid'	= getBaseTID via
		if isNothing viaTid' then return False else do	-- the via is some strange case of "a -> b" or so. We ignore it, as it can not be added anyway
		let viaTid	= fromJust viaTid'
		{- oldBinding converts via/changedSuper to the basetype
		Z	is Y Bool
		_expand "Z" "Y Bool"
		oldBinding = {a0 --> Bool} and will take the super "Y a0" to
			the applied form "Y Bool", which "Z" is
		-}
		let getBinding (_,_,tb)	= snd tb
		let oldBinding	= fetch (show via) "fstt" via fstt & getBinding
					:: Binding
		-- the supers FSTT, which we will tear apart and add to base
		supersToAdd	<- get' fstt_ |> findWithDefault M.empty viaTid
		let supersTA	= supersToAdd & M.toList |> (\(s,b) -> (s,getBinding b))
		mapM (uncurry $ _addEntry base via oldBinding) supersTA |> or

_addEntry	:: TypeID -> RType -> Binding -> RType -> Binding -> St Bool
_addEntry base via oldBinding superToAdd newBinding
	= do	fstts		<- get' fstt_
		-- the fstt that has to be changed
		let fstt	= findWithDefault M.empty base fstts
		-- the new, combined binding ...
		let binding	= concatBindings oldBinding newBinding
		-- ... aplied on the super
		let super	= substitute binding superToAdd
		if super `M.member` fstt then return False else do
		-- we just use the reqs of the via type ...
		let reqs	= M.lookup via fstt |> (\(reqs, _, _) -> reqs) & fromMaybe []
		{- .. on which we substitute binding. Some requirements can dissapear.
		 e.g. List (a:Eq) -> Eq
			X is List Bool
			-> X is Eq <-> Bool is Eq -> ok
			We check those later, when we can bind

		-}
		let reqs'	= reqs |> subReq binding & catMaybes
		let entry	= (reqs, Just via, (superToAdd, binding))
		let fstt'	= M.insert super entry fstt
		let fstts'	= M.insert base fstt' fstts
		modify (\ctx -> ctx {fstt_ = fstts'})
		return True


subReq	:: Binding -> (Name, Set RType) -> Maybe (Name, Set RType)
subReq _	= Just	-- TODO


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
