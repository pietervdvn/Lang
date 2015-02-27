module Languate.TypeTable.Checks.CheckReqTable (validateReqTable) where

import StdDef
import MarkDown
import Exceptions
import Languate.CheckUtils

import qualified Data.Set as S
import Data.Maybe
import Data.Map hiding (map)

import Languate.TAST
import Languate.TypeTable
import Languate.TypeTable.Checks.CheckKind
import Languate.TypeTable.KindChecker.KindOf

import Data.List hiding (lookup, insert)
import Prelude hiding (lookup)

-- Checks that the requirements have the same kind. E.g. ''k:Eq, Mappable'' is wrong. (but ''a:Monad, Mappable'' is allowed)
validateReqTable	:: Map TypeID (Map Int Name) -> KindLookupTable -> TypeReqTable -> Check
validateReqTable freeNms klt tr
	=  do	let toCheck	= keys tr
		mapM_ (uncurry $ checkReqsFor freeNms klt tr) toCheck


checkReqsFor	:: Map TypeID (Map Int Name) -> KindLookupTable -> TypeReqTable -> TypeID -> Int -> Check
checkReqsFor freeNms klt treqs typeId i
	= inside ("While checking the type requirements of "++show typeId) $ try err $
	   do	let name	= nameFor freeNms typeId i
		let shouldBeT	= reqsFor treqs typeId i
		inside ("While checking the "++count (1 +i)++" type variable, namely '"++name++"'") $ do
			freeTable	<- buildFreeTable klt freeNms treqs typeId [0..i] empty
			kinds	<- mapM (kindOf klt freeTable) shouldBeT
			assert (allSame kinds) $ indent' ("The free type variable '"++name++"' has requirements of different kinds: ") $
				intercalate "\n" (map (\(k,t) -> show t ++ ":: "++show k) $ zip kinds shouldBeT)

-- Builds a table of the form ''{ k -> *, v -> *~>* }. Assumes the free has the kind of the first requirements
buildFreeTable	:: KindLookupTable -> Map TypeID (Map Int Name) -> TypeReqTable -> TypeID -> [Int] -> Map Name Kind -> Exc (Map Name Kind)
buildFreeTable _ _ _ _ [] currentTable
			= return currentTable	-- no more argument to check
buildFreeTable klt freeNms treqs tId (i:is) currentTable
			=  do	let rtyp	= listToMaybe $ reqsFor treqs tId i
				let n	= nameFor freeNms tId i
				case rtyp of
					Nothing	-> 		buildFreeTable klt freeNms treqs tId is $ insert n Kind currentTable
					(Just rt)	-> do	kind	<- kindOf klt currentTable rt
								buildFreeTable klt freeNms treqs tId is $ insert n kind currentTable

reqsFor	:: TypeReqTable -> TypeID -> Int -> [RType]
reqsFor treqs typeId i	= fromMaybe [ ] (lookup (typeId, i) treqs |> S.toList)

nameFor	:: Map TypeID (Map Int Name) -> TypeID -> Int -> Name
nameFor freeNms tId i 	= fromMaybe "bug" $ lookup tId freeNms >>= lookup i
