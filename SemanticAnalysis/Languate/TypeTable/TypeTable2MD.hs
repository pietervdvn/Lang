module Languate.TypeTable.TypeTable2MD where

import StdDef
import MarkDown

import Data.Map (keys, Map, lookup, findWithDefault, empty)
import qualified Data.Set as S
import Prelude hiding (lookup)
import Data.List (sort)

import Languate.FQN
import Languate.TypeTable
import Languate.TAST



-- Build a simple markdown overview of the type table
typeTable2md	:: TypeTable -> MarkDown
typeTable2md tt	= (table ["Type","Declared in","Kind","Requirements","Docstring"] $ map (typeRow tt) $ keys $ kinds tt)

typeRow		:: TypeTable -> TypeID -> [MarkDown]
typeRow	tt (fqn, name)
		=  let getMaybe table	= lookup (fqn, name) $ table tt in
			[ bold name ++ unwords (frees tt (fqn, name))
			, showShortFQN fqn
			, maybe (bold "ERROR: no kind found") show $ getMaybe kinds
			, typeReqsFor tt (fqn, name)
			, recode $ maybe "" firstLine $ getMaybe docstrings
			]


frees		:: TypeTable -> TypeID -> [Name]
frees tt id	=  let  freeNmT	= findWithDefault empty id $ freeNames tt
			kys	= sort $ keys freeNmT	in
			map (\i -> findWithDefault "" i freeNmT) kys


typeReqsFor	:: TypeTable -> TypeID -> MarkDown
typeReqsFor tt id
	= let	freeNmT	= findWithDefault empty id $ freeNames tt
		kys	= sort $ keys freeNmT in
		intercal "; " $ map (typeReqFor tt id freeNmT) kys


typeReqFor	:: TypeTable -> TypeID -> Map Int Name -> Int -> MarkDown
typeReqFor tt id names i
	= let 	name	= findWithDefault "?" i names
		reqs	= findWithDefault S.empty (id,i) $ typeReqs tt in
		when (name ++":") $ intercal ", " $ map showShort $ S.toList reqs


showShort	:: RType -> String
showShort	=  st True
