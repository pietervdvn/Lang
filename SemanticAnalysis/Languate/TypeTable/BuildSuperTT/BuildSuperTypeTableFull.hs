 module Languate.TypeTable.BuildSuperTT.BuildSuperTypeTableFull where

import StdDef hiding (todo)

import Languate.TAST
import Languate.TypeTable

import Prelude hiding (null)
import Data.Set as S hiding (null, filter)
import Data.Map hiding (filter)
import qualified Data.Map as M
import qualified Data.List as L
import Data.Either (rights)
import Data.Tuple
import Data.Maybe

{-- converts a simple super type table into a (incomplete) full super type table
These tables should still be ''expand''ed
-}
stt2fstt	:: SuperTypeTableFor -> FullSuperTypeTable
stt2fstt sttf
	= let tuples	= unmerge (M.toList sttf ||>> S.toList) |> conv in
		M.fromList $ concat tuples

-- creates the base entries for the fstt
conv	:: ([Name], (RType, Map Name [RType])) -> [FSTTKeyEntry]
conv (frees, (isTyp, reqs))
	= let 	nativeS	= frees |> (\a -> findWithDefault [] a reqs) |> S.fromList
		-- remove all frees which have been used from isTyp
		-- isTyp	= fst $ substitute' frees isTyp'
		-- we want to bind "X Bool" against "X a", to derive {a --> Bool}
		binding	= Binding $ canonicalBinding isTyp
		base	= (isTyp, FSTTEntry (zip frees nativeS) Nothing isTyp binding Nothing) in
		base:expandFrees reqs base


{-
If the rtype is a free, and requirements are known about this free, we can say more about the supertype.

e.g.

cat Weighted (graph:Graph)	: graph

We will get it in as:
(RFree "graph", [(graph:{Graph, X, Y})])
and give:
[(Graph, [graph:Graph, X, Y])]
[(X, [graph:Graph, X, Y])]
[(Y, [graph:Graph, X, Y])]

-}
expandFrees	:: Map Name [RType] -> FSTTKeyEntry -> [FSTTKeyEntry]
expandFrees reqs (RFree a, rest)
	= let possible	= findWithDefault [] a reqs in
		zip possible $ repeat rest
expandFrees reqs (RApplied bt at, rest)	-- the argument type (at) is always a free
	= expandFrees reqs (bt, rest) |> swap ||>> (`RApplied` at) |> swap
expandFrees _ _	= []
