module Languate.TypeTable.TypeTable2MD where

import StdDef
import MarkDown

import Data.Map (keys, Map, lookup, findWithDefault, empty, toList)
import qualified Data.Set as S
import Prelude hiding (lookup)
import Data.List (sort)

import Languate.FQN
import Languate.TypeTable
import Languate.TAST

import Languate.TypeTable.FullSuperTypeTable2MD



-- Build a simple markdown overview of the type table
typeTable2md	:: TypeTable -> MarkDown
typeTable2md tt
	= let	overviewTable	= (table ["Type","Declared in","Kind","Docstring"] $ map (typeRow tt) $ keys $ kinds tt)
		sttTitle	= title 2 "Supertypes " ++ parag (superTypeTable2md tt)
		perType	= ((toList $ allSupertypes tt) >>= (uncurry (fstt2md $ kinds tt)))
		in overviewTable ++ sttTitle ++ title 2 "Supertypetable per type" ++ explanation ++ perType


typeRow		:: TypeTable -> TypeID -> [MarkDown]
typeRow	tt (fqn, name)
		=  let getMaybe table	= lookup (fqn, name) $ table tt in
			[ bold name ++ typeReqsFor tt (fqn, name)
			, showShortFQN fqn
			, code $ maybe (bold "ERROR: no kind found") show $ getMaybe kinds
			, recode $ maybe "" firstLine $ getMaybe docstrings
			]


-- Builds a string as   ````a:Eq```` for Set a:Eq in supertypetable
typeReqsFor	:: TypeTable -> TypeID -> MarkDown
typeReqsFor tt id
	= let	freeNmT	= findWithDefault empty id $ freeNames tt
		kys	= sort $ keys freeNmT in
		unwords $ map (typeReqFor tt id freeNmT) kys


typeReqFor	:: TypeTable -> TypeID -> Map Int Name -> Int -> MarkDown
typeReqFor tt id names i
	= let 	name	= findWithDefault "?" i names
		reqs	= findWithDefault S.empty (id,i) $ typeReqs tt in
		code $ name ++ when ":" (strip $ intercal "," $ map (strip . showShort) $ S.toList reqs)


showShort	:: RType -> String
showShort	=  st True


-- Supertypetable --
--------------------

superTypeTable2md	:: TypeTable -> MarkDown
superTypeTable2md tt	=  let	all	= keys $ supertypes tt	in
				table ["Type","Is subtype of"] $ concatMap (superType2md tt) all

superType2md	:: TypeTable -> TypeID -> [[MarkDown]]
superType2md tt tid
		= let 	all	= superTypesFor tt tid
			showTps	tps	= intercal ", " $ (tps & filter (/= ". ") |> code) 	:: MarkDown
			entries	= all |> showEntry tid & merge ||>> showTps & filter (not . null . snd) |> (\(t,tps) -> [t,tps]) in
			entries



showEntry	:: TypeID -> ([Name], RType, Map Name [RType]) -> (MarkDown, MarkDown)
showEntry (_, n) (frees, super, freeReqs)
		= ( n ++ showReqs (\n -> findWithDefault [] n freeReqs) frees
		  , showShort super
		  )

showReqs	:: (Name -> [RType]) -> [Name] -> MarkDown
showReqs f	= intercal " " . map (showReq f)


showReq		:: (Name -> [RType]) -> Name -> MarkDown
showReq f free	= code $ free ++ when ":" (intercal ", " $ map showShort $ f free)
