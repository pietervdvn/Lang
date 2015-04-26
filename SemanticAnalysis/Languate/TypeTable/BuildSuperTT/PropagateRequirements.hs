module Languate.TypeTable.BuildSuperTT.PropagateRequirements (propagateRequirements) where

import StdDef
import Exceptions
import Languate.CheckUtils
import State

import Data.Map (Map, findWithDefault)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Maybe
import Graphs.DirectedGraph as DG


import Languate.TypeTable
import Languate.TAST
import Languate.TypeTable.BuildSuperTT.FixImplicitRequirements
import Languate.TypeTable.Bind.Substitute
import Languate.TypeTable.Bind.Bind

import Control.Monad

import Debug.Trace
{--

When the super type table is built, some extra requirement might become clear.

E.g.


Collection a0 is Eq if (a0:Eq)

Dict k1 v1 is Collection (k1,v1)

This means that
Dict k1 v1 is Eq if ( (k1,v1) is Eq)

As (k1,v1) is Eq whenever k1:Eq and v1:Eq, this implies

Dict k1 v1 is eq if (k1:Eq) and (v1:Eq).

These indirect type requirements are added here to the FSTTs.
Then all toBinds are checked and exceptions generated.
--}
propagateRequirements	:: SpareSuperTypeTables -> DG TypeID -> ToBinds -> FullSuperTypeTables -> KindLookupTable -> Exc FullSuperTypeTables
propagateRequirements sstts notifyTable toBinds fstts klt
	= let 	ctx	= Context fstts sstts klt notifyTable []
		-- If a type does not contain a free, no requiments can be deduced
		-- toBinds' contains all the toBinds which might contain a free
		toBinds'= [STB subTid isSuper ifThisType isThis
				| ToBnd subTid isSuper ifThisType isThese _ <- toBinds
				, isThis <- S.toList isThese
				, not (null $ freesInRT ifThisType)]
		fstts'	= runstate (propagateReqs toBinds') ctx & snd & fstts_ in do
		-- check if we really met all requirements
		checkSuperBinds fstts' sstts klt toBinds
		return fstts'

data SingleToBind
		= STB {	subTid	:: TypeID,
			isSuper	:: RType,
			ifThisType	:: RType,
			isThisOne	:: RType}
	deriving (Show)
data Context	= Context 	{ fstts_	:: FullSuperTypeTables
				, sstts_	:: SpareSuperTypeTables
				, kinds_	:: KindLookupTable
				-- queue representing: This fstt should be rechecked, as this fstt has a changed requirement for this supertype
				, notifyTable ::  DG TypeID
				, queue	:: [(TypeID, (TypeID, TypeID))]}

type StMsg a	= State Context a

{-
Adds the requirements, until not a single toBind gave a change in the fstts
-}
propagateReqs	:: [SingleToBind] -> StMsg ()
propagateReqs toBinds
	= do	somethingChanged	<- mapM propagateReq toBinds |> or
		when somethingChanged $ propagateReqs toBinds

{- Adds the requiments deduced in tobind to the core fstt, propagates those through the FSTT.
	When this propagation is done, checks if more requirements should be deduced from this to bind.
	Returns true if somethingChanged.
-}
propagateReq	:: SingleToBind -> StMsg Bool
propagateReq  toBind@(STB baseSubTid baseSuper sub super)	-- We call ifThisType "sub" and isThisOne "super"! Do not get confused!
 | not (isNormal sub) || not (isNormal super)
	= return False	-- we ignore strange cases here
 | otherwise
	= do	{- The sub should be a element for super.
			We lookup the sub in the fssts, build binding instance -> canonical form,
			get the requiments of it, and see if more requirements are needed -}
		let subTid	= getBaseTID sub & fromJust
		let superTid	= getBaseTID super & fromJust
		fstts	<- get' fstts_
		sstts	<- get' sstts_
		klt	<- get' kinds_
		subFstt	<- get' fstts_ |> findWithDefault M.empty subTid
		subSstt	<- get' sstts_ |> findWithDefault M.empty subTid
		{- We lookup the super (of which we need the requirements of) in the SSTT
			If no supertype shows up, we know that this requirement can never be met, and we stop here.
			This missing requirement will be found in the checks afterwards-}
		if superTid `M.notMember` subSstt then return False else do
		let posSuperForms= findWithDefault [] superTid subSstt
		-- fisrt we check if the requiment can be met right now against one of the possible super types
		let baseSuperEntry
			= fstts & findWithDefault M.empty baseSubTid & M.lookup baseSuper
		-- base super is not configured correctly, as all frees are already in the right context
		if isNothing baseSuperEntry then return False else do
		let knownFreeReqs
			= baseSuperEntry & fromMaybe (error $ "No basesuperentry for "++show baseSuper++" in fstt of "++show baseSubTid) & reqs & M.fromList
		let alreadyMet	= map (_bind sstts fstts klt (knownFreeReqs |> S.toList) sub) posSuperForms |> isRight & or
		if alreadyMet then return False else do
		possReqs	<- forM posSuperForms $ \posSuperForm ->
		  do	-- we calculate here what **extra** requirements are needed to meet the posSuperForm
			let fsstEntry	= M.lookup posSuperForm subFstt & fromMaybe (error $ "PropagateRequirements: "++show posSuperForm++" not found in subFstt of "++show sub)
			let oldReqs	= reqs fsstEntry
			-- what requiments didn't we meet?
			let missingReqs	= calculateMissingReqs oldReqs knownFreeReqs
			return missingReqs
		{- did we find possible, new requirement config such that the supertype is met? If not -> return false, the postcheck will figure it out	-}
		if null possReqs then return False else do
		-- what is the best? The one with the least new free types with requiments
		let chosenReq	= head $ sortOn length possReqs
		addRequirement baseSubTid baseSuper chosenReq
		return True

calculateMissingReqs	:: [(Name, Set RType)] -> Map Name (Set RType) ->
				[(Name, Set RType)]
calculateMissingReqs oldReqs newReqs
	= oldReqs |> (\(nm, oldKnown) -> (nm,oldKnown `S.difference` findWithDefault S.empty nm newReqs)) & filter (not . S.null . snd)


{- Adds the requiment (name, {RType}) to the FSTT of tid, for the supertype RType
	Will add fstts to the queue that should be re-evaluated, and propage these through the fstts. This propagation might result in another missed requiment, but that is for the next iteration of the main loop! -}
addRequirement	:: TypeID -> RType -> [(Name, Set RType)] -> StMsg ()
addRequirement subTid super req
	= do	origReqs 	<- get' fstts_  |> (M.lookup subTid >=> M.lookup super)
					|> fromMaybe (error "PropagateRequirements: derp?")
					|> reqs
		let newReqs	= (origReqs ++ req) & merge ||>> S.unions
		when (newReqs /= origReqs) $ do
		modify . modFstts_ . flip M.adjust subTid . flip M.adjust super $
			(\fsttEntry -> fsttEntry {reqs = newReqs})



-- CHECKS --
------------


{-
During the calculation of the FSTT, values might have been bound where type reqs are present.

e.g.
RSA is PubPrivAlgo RSAPrivKey RSAPubKey

This means that RSAPrivKey should be a PrivKey. This requirements are checked here.

-}

checkSuperBinds	:: Map TypeID FullSuperTypeTable -> Map TypeID SpareSuperTypeTable -> KindLookupTable -> ToBinds -> Check
checkSuperBinds fstts sstts klt toCheck
	= inside "While checking that implicit type requirements are met" $ do
		let toCheck'	= toCheck & filter (not . S.null . neededReqs)
		mapM_ (checkSuperBinds' fstts sstts klt) toCheck'

checkSuperBinds'	:: Map TypeID FullSuperTypeTable -> Map TypeID SpareSuperTypeTable -> KindLookupTable -> ToBind -> Check
checkSuperBinds' fstts sstts klt (ToBnd subTid superT sub shouldBeSupers msg)
 = inside msg $ do
	let metReqs	= findWithDefault M.empty subTid fstts
				& M.lookup superT & fromMaybe (error $ "PropagateRequirements: No entry for "++show superT++" in the fstt of "++show subTid++"\n"++msg) & reqs
				& M.fromList |> S.toList
	let checkOne	= _bind sstts fstts klt metReqs sub
	let showMsg super msg
		= inside ("The type requirement '"++st True sub++" is a "++st True super++"' could not be fullfilled") $ err msg
	let checkOne' super	= either (showMsg super) (const pass) $ checkOne super
	mapM_ checkOne' $ S.toList shouldBeSupers



modFstts_	:: (FullSuperTypeTables -> FullSuperTypeTables) -> Context -> Context
modFstts_ f ctx	= ctx {fstts_ = f $ fstts_ ctx}
setFstts fstts	= modFstts_ (const fstts)


modFsttEntry	:: (TypeID, RType) -> (FSTTEntry -> FSTTEntry) -> Context -> Context
modFsttEntry (tid, rt) f
		= modFstts_ (M.adjust (M.adjust f rt) tid)
setFsttEntry tidrt entry
		= modFsttEntry tidrt (const entry)
getFsttEntry (tid, rt) ctx
		= ctx & fstts_ & findWithDefault (error $ "No fstt found for tid "++show tid) tid & findWithDefault (error $ "No entry found for "++show rt++"; super "++show rt) rt

modSstts_	:: (SpareSuperTypeTables -> SpareSuperTypeTables) -> Context -> Context
modSstts_ f ctx	= ctx {sstts_ = f $ sstts_ ctx}
setSstts sstts	= modSstts_ (const sstts)

modqueue	:: ([(TypeID, (TypeID, TypeID))] -> [(TypeID, (TypeID, TypeID))]) -> Context -> Context
modqueue f ctx	= ctx {queue = f $ queue ctx}
setqueue queue	= modqueue (const queue)
