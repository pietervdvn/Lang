module Languate.TypeTable.BuildSuperTT.FixImplicitRequirements where

import StdDef
import Data.Map hiding (null)
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Either
import Data.Maybe

import Languate.TAST
import Languate.TypeTable
import Languate.TypeTable.Bind.Substitute

import Control.Monad.Writer
import Control.Arrow

{- Represents a requiment which is not met at this moment in the algorithm, but should be met.
We postpone the check to the moment the entire supertypetable is checked
-}
data ToBind	= ToBnd	{ subType	:: TypeID	-- this type
			, superType	:: RType	-- has this supertype
			, ifType	:: RType	-- if this type
			, neededReqs	:: Set RType	-- meets this requiments
			, msg		:: Message	-- a message for the human
			}
	deriving (Show)
type ToBinds	= [ToBind]

fixImplicitRequirements	:: TypeReqTable -> FullSuperTypeTables ->
				(FullSuperTypeTables, ToBinds)
fixImplicitRequirements treqt
		= runWriter . fixImplicitRequirements_ treqt


fixImplicitRequirements_	:: TypeReqTable -> FullSuperTypeTables ->
				Writer ToBinds FullSuperTypeTables
fixImplicitRequirements_ treqt fstts
	= mapM (uncurry $ fixImplicitsFor treqt fstts) (M.toList fstts) |> M.fromList


fixImplicitsFor	:: TypeReqTable -> Map TypeID FullSuperTypeTable ->
			TypeID -> FullSuperTypeTable ->
			Writer ToBinds (TypeID, FullSuperTypeTable)
fixImplicitsFor treqt fstts tid fstt
	= mapM (uncurry $ fixImplicitsForEntry tid treqt fstts) (M.toList fstt)
		|> M.fromList |> (const tid &&& id)

{- Gets and adds the extra requirements for native types.

Gives a (this type, should be these types) too, when binding happens on the way.
-}
fixImplicitsForEntry	:: TypeID -> TypeReqTable -> Map TypeID FullSuperTypeTable ->
				RType -> FSTTEntry -> Writer ToBinds FSTTKeyEntry
fixImplicitsForEntry tid treqt fstts rtype entry
				-- do nothing if the super type is ""a -> b""
 | not $ isNormal $ origSuper entry	= return (rtype, entry)
 | otherwise
	= do	let origType	= origSuper entry
		let origTypeID	= getBaseTID origType & fromMaybe (error $ "FixImplicitRequirements: "++show origType++" is not normal")
		let origTypeRqs	= findWithDefault [] origTypeID treqt	:: [(Name, Set RType)]
		let (toCheck, extraReqs)= origTypeRqs |> subReq (origBinding entry) & (lefts &&& rights)
		let msg	= "While adding the implicit requirements on "++show tid++" for its supertype "++show rtype
		let fullReqs	= (extraReqs ++ reqs entry) & merge ||>> S.unions & Prelude.filter (not . S.null . snd)
		unless (null fullReqs) $ tell $ toCheck |> (\(ifType, isTypes) -> ToBnd tid rtype ifType isTypes msg)
		return (rtype, entry {reqs = fullReqs})


subReq		:: Binding -> (Name, Set RType) -> Either (RType, Set RType)  (Name, Set RType)
subReq binding@(Binding dict) (nm, supers)
	= let supers'	= S.map (substitute binding) supers in
		case M.lookup nm dict of
			(Just (RFree nm'))	-> Right (nm', supers')
			-- nm corresponds with a bound type -> check binding
			(Just tp)		-> Left (tp, supers')
			Nothing			-> Right (nm, supers')
