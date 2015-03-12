module Languate.TypeTable.BuildSuperTT.FixImplicitRequirements where

import StdDef
import Data.Map
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe

import Languate.TAST
import Languate.TypeTable
import Languate.TypeTable.Extended
import Languate.TypeTable.Bind.Substitute

import Control.Monad.Writer

type ToBinds	= [(RType, Set RType)]

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
	= mapM (uncurry $ fixImplicitsForEntry treqt fstts) (M.toList fstt)
		|> M.fromList |> \fstt -> (tid, fstt)

{- Gets and adds the extra requirements for native types.

Gives a (this type, should be these types) too, when binding happens on the way.
-}
fixImplicitsForEntry	:: TypeReqTable -> Map TypeID FullSuperTypeTable ->
				RType -> FullSTTEntry -> Writer ToBinds FullSTTKeyEntry
fixImplicitsForEntry treqt fstts rtype entry@(nameReqs, via, (origType, bnd))
				-- do nothing if the super type is ""a -> b""
 | not $ isNormal origType	= return (rtype, entry)
 | otherwise
	= do	let origTypeID	= getBaseTID origType & fromJust
		let origTypeRqs	= findWithDefault [] origTypeID treqt
					:: [(Name, Set RType)]
		extraReqs	<- mapM (subReq bnd) origTypeRqs |> catMaybes
		let fullReqs	= (extraReqs ++ nameReqs) & merge ||>> S.unions
		return (rtype,(fullReqs, via, (origType, bnd)))


subReq		:: Binding -> (Name, Set RType) -> Writer ToBinds (Maybe (Name, Set RType))
subReq binding@(Binding dict) (nm, supers)
	= case M.lookup nm dict of
		(Just (RFree nm'))	-> return $ Just (nm', S.map (substitute binding) supers)
		-- nm corresponds with a bound type -> check binding
		(Just tp)		-> tell [(tp, supers)] >> return Nothing
		Nothing	-> return $ Just (nm, supers)
