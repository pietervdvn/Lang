module Languate.TypeTable.Checks.CheckReqTable (validateReqTable) where

import StdDef
import MarkDown
import Exceptions
import Languate.CheckUtils

import Data.Set (Set)
import qualified Data.Set as S
import Data.Maybe
import Data.Map hiding (map)
import qualified Data.List as L

import Languate.TAST
import Languate.TypeTable
import Languate.TypeTable.Checks.CheckKind
import Languate.TypeTable.KindChecker.KindOf

import Data.List hiding (lookup, insert)
import Prelude hiding (lookup)

-- Checks that the requirements have the same kind. E.g. ''k:Eq, Mappable'' is wrong. (but ''a:Monad, Mappable'' is allowed)
validateReqTable	:: KindLookupTable -> TypeReqTable -> Check
validateReqTable klt treqt
	= mapM_ (uncurry $ validateReqs klt) $ toList treqt

validateReqs	:: KindLookupTable -> TypeID -> [(Name, Set RType)] -> Check
validateReqs klt tid reqs
	= inside ("In the type requirements for the type "++show tid) $ do
		freeKinds 	<- bindKind' klt tid (reqs |> fst) |> snd
		mapM_ (uncurry $ validateOneReq klt freeKinds) $ zip [1..] reqs


validateOneReq	:: KindLookupTable -> Map Name Kind -> Int -> (Name, Set RType) -> Check
validateOneReq klt freeKinds i (nm, reqs)
	= inside ("In the "++count i++" type requirement, namely '"++nm++"'") $
	  inside ("Some of the requirements have different kinds") $
	  do	let rqs	= S.toList reqs
		kinds	<- mapM (kindOf klt freeKinds) rqs
		if L.null kinds then pass else do
		let allSame	= all ((==) $ head kinds) $ tail kinds
		let showReq rq kind	= show rq ++ " has the kind "++show kind
		assert allSame $ zip rqs kinds |> uncurry showReq & unlines
