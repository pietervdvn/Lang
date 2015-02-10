module Languate.TypeTable.BuildSuperTypeTableFull where

import StdDef hiding (todo)

import Languate.TAST
import Languate.TypeTable
import Languate.TypeTable.Bind.Bind (unificate, simpleBind, joinEither)
import Languate.TypeTable.Bind.Substitute

import Prelude hiding (null)
import Data.Set as S hiding (null, filter)
import Data.Map hiding (filter)
import qualified Data.Map as M
import qualified Data.List as L
import Data.Either (rights)
import Data.Tuple
import Data.Maybe

import Languate.Graphs.DirectedGraph
import State
import Control.Monad

import Debug.Trace

-- converts a simple super type table into a full super type table
stt2fstt	:: SuperTypeTableFor -> FullSuperTypeTable
stt2fstt sttf
	= let tuples	= unmerge (M.toList sttf ||>> S.toList) |> conv in
		M.fromList $ concat tuples


-- creates the base entries for the fstt
conv	:: ([Name], (RType, Map Name [RType])) ->
		[(RType, ([(Name, Set RType)], Binding, Maybe RType) )]
conv (frees, (isTyp, reqs))
	= let 	nativeS	= frees |> (\a -> findWithDefault [] a reqs) |> S.fromList
		-- remove all frees which have been used from isTyp
		-- isTyp	= fst $ substitute' frees isTyp'
		-- we want to bind "X Bool" against "X a", to derive {a --> Bool}
		applied	= appliedTypes isTyp
		binding	= Binding $ M.fromList $ zip ([0..] |> show |> ('a':)) applied
		base	= (isTyp, (zip frees $ nativeS, binding, Nothing)) in
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
expandFrees	:: Map Name [RType] ->
			 (RType, ([(Name, Set RType)], Binding, Maybe RType)) ->
			[(RType, ([(Name, Set RType)], Binding, Maybe RType))]
expandFrees reqs (RFree a, rest)
	= let possible	= findWithDefault [] a reqs in
		zip possible $ repeat rest
expandFrees reqs (RApplied bt at, rest)	-- the argument type (at) is always a free
	= expandFrees reqs (bt, rest) |> swap ||>> (\bt -> RApplied bt at) |> swap
expandFrees _ _	= []





{-
Makes the super type table complete, by recursively adding the supertypes of (known) super types.
-}
expand	:: Map TypeID FullSuperTypeTable -> Map TypeID FullSuperTypeTable
expand 	= id



buildSpareSuperTypeTable	:: FullSuperTypeTable -> SpareSuperTypeTable
buildSpareSuperTypeTable dict
	= dict & keys |> (\a -> (a,a)) ||>> getBaseTID
		|> swap |> unpackMaybeTuple & catMaybes	-- removing failed lookups
		& merge & M.fromList
