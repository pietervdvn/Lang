module Languate.TypeTable.BuildSuperTT.ExpandFSTT where

{--
This module implements the FSTT expansion
--}

import StdDef hiding (todos)
import Exceptions

import Languate.CheckUtils

import Languate.TAST
import Languate.TypeTable
import Languate.TypeTable.BuildSuperTT.FixImplicitRequirements
import Languate.TypeTable.BuildSuperTT.PropagateRequirements
import Languate.TypeTable.Bind.Substitute

import Prelude hiding (null)
import Data.Set hiding (null, filter)
import qualified Data.Set as S
import Data.Map hiding (filter)
import qualified Data.Map as M
import qualified Data.List as L
import Data.Either
import Data.Tuple
import Data.Maybe

import Graphs.DirectedGraph

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
expand	:: (Map TypeID FullSuperTypeTable, ToBinds) ->
		Exc (Map TypeID FullSuperTypeTable, Map TypeID SpareSuperTypeTable)
expand (fstt, toCheck')
	= let 	initSstt	= fstt |> buildSpareSuperTypeTable
		initNotifTable	= initSstt	|> keys |> S.fromList & invertDict
		initTodo	= fstt 		|> keys |> S.fromList
		ctx	= Ctx fstt initNotifTable initSstt initTodo toCheck'
		ctx'	= runstate _expandAll ctx & snd
		-- stuff that has to be checked, as bind aways might have happened
		toBind	= toCheck ctx'
		fstts	= fstt_ ctx'
		sstts	= sstt_ ctx' in do
		fstts'	<-propagateRequirements sstts (notifyTable ctx') toBind fstts
		return (fstts', sstts)




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
		-- These are requirements that should be met. The third arg is the context in which this is bound
		toCheck	:: ToBinds }




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
	-- changedSuper is some strange case -> we do not add it (this can happen with the initial settings to start the algo)
 |  (base == anyTypeID) || not (isNormal changedSuper)
	= return False
 | otherwise
   = do	-- convenience function to get the binding out of a FSSTentry
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
	let oldBinding	= fetch (show via) "fstt" via fstt & origBinding
				:: Binding
	-- the supers FSTT, which we will tear apart and add to base
	supersToAdd	<- get' fstt_ |> findWithDefault M.empty viaTid |> M.toList
	mapM (_addEntry base via oldBinding) supersToAdd |> or


{-

Adds a supertypeentry to the basetype. The oldbinding takes the supertype(via) from its baseform to the needed form,

-}
_addEntry	:: TypeID -> RType -> Binding -> FSTTKeyEntry -> St Bool
_addEntry base via oldBinding (superToAdd, entry)
  = do	fstts		<- get' fstt_
	-- the fstt that has to be changed
	let fstt	= findWithDefault M.empty base fstts
	-- the new, combined binding ...
	let binding	= concatBindings oldBinding $ origBinding entry
	-- ... aplied on the super
	let super	= substitute binding superToAdd
	-- if already added: skip this shit
	if super `M.member` fstt then return False else do
	{- We calculate the requirements after substitution. These are the requirements we get from the foreign FSTT
		It is tested against the full binding -}
	let (toCheck, foreignReqs)
			= reqs entry |> subReq binding & (lefts &&& rights)
	{- We might have some requirements on the via type too in our own FSTT.
		These don't have to be checked, as these are already requirements in function of the frees of base -}
	let viaReqs	= fstt & M.lookup via |> reqs & fromMaybe []
	let newReqs	= (foreignReqs ++ viaReqs) & merge ||>> S.unions & filter (not . S.null . snd)
	-- add the tochecks
	let msg	= "In the expansion of the supertype table of "++show base++" (adding the super "++show super++" which has been added via "++show via++")"
	mapM_ addReqs $ toCheck |> (\(ifType, isTypes) ->
		ToBnd base super ifType isTypes msg)
	let entry	= FSTTEntry newReqs (Just via) superToAdd binding (Just $ origBinding entry)	-- TODO or oldbinding?
	let fstt'	= M.insert super entry fstt
	let fstts'	= M.insert base fstt' fstts
	modify (\ctx -> ctx {fstt_ = fstts'})
	let mSuperTid	= getBaseTID superToAdd
	-- adds notifications. If mSuperTid does not exists -> superToAdd is a curry -> No notifications needed anyway
	when (isJust mSuperTid) $ notifyMe base (fromJust mSuperTid, superToAdd)
	return True	-- somethingChanged -> we return true


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

addReqs		:: ToBind -> St ()
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
