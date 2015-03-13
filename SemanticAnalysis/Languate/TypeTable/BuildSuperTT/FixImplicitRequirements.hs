module Languate.TypeTable.BuildSuperTT.FixImplicitRequirements where

import StdDef
import Data.Map
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Either
import Data.Maybe

import Languate.TAST
import Languate.TypeTable
import Languate.TypeTable.Extended
import Languate.TypeTable.Bind.Substitute

import Control.Monad.Writer
import Control.Arrow

type ToBind	= (RType, Set RType, [(Name, Set RType)], Message)
type ToBinds	= [ToBind]

fixImplicitRequirements	:: TypeReqTable -> Map TypeID FullSuperTypeTable ->
				(Map TypeID FullSuperTypeTable, ToBinds)
fixImplicitRequirements treqt
		= runWriter . fixImplicitRequirements_ treqt


fixImplicitRequirements_	:: TypeReqTable -> Map TypeID FullSuperTypeTable ->
				Writer ToBinds (Map TypeID FullSuperTypeTable)
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
				RType -> FullSTTEntry -> Writer ToBinds FullSTTKeyEntry
fixImplicitsForEntry tid treqt fstts rtype entry@(nameReqs, via, (origType, bnd))
				-- do nothing if the super type is ""a -> b""
 | not $ isNormal origType	= return (rtype, entry)
 | otherwise
	= do	let origTypeID	= getBaseTID origType & fromJust
		let origTypeRqs	= findWithDefault [] origTypeID treqt
					:: [(Name, Set RType)]
		let (toCheck, extraReqs)= origTypeRqs |> subReq bnd & (lefts &&& rights)
		let msg	= "While adding the implicit requirements on "++show tid++" for its supertype "++show rtype
		let fullReqs	= (extraReqs ++ nameReqs) & merge ||>> S.unions & Prelude.filter (not . S.null . snd)
		tell $ fmap (\((a,b),c) -> (a,b,c, msg)) $ zip toCheck $ repeat fullReqs
		return (rtype,(fullReqs, via, (origType, bnd)))


subReq		:: Binding -> (Name, Set RType) -> Either (RType, Set RType)  (Name, Set RType)
subReq binding@(Binding dict) (nm, supers)
	= let supers'	= S.map (substitute binding) supers in
		case M.lookup nm dict of
			(Just (RFree nm'))	-> Right (nm', supers')
			-- nm corresponds with a bound type -> check binding
			(Just tp)		-> Left (tp, supers')
			Nothing			-> Right (nm, supers')
