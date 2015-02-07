module Languate.TypeTable.BuildSuperTypeTableFull where

import StdDef hiding (todo)

import Languate.TAST
import Languate.TypeTable

import Prelude hiding (null)
import Data.Set as S hiding (null, filter)
import Data.Map hiding (filter)
import qualified Data.Map as M
import qualified Data.List as L

import Data.Tuple
import Data.Maybe

import Languate.Graphs.DirectedGraph
import State
import Control.Monad

-- converts a simple super type table into a full super type table
stt2fstt	:: SuperTypeTableFor -> FullSuperTypeTable
stt2fstt sttf
	= let tuples	= unmerge (M.toList sttf ||>> S.toList) |> conv in
		M.fromList $ concat tuples



conv	:: ([Name], (RType, Map Name [RType])) -> [(RType, [(Name, Set RType)])]
conv (frees, (isTyp, reqs))
	= let base	= (isTyp, zip frees $ frees |> (\a -> findWithDefault [] a reqs) |> S.fromList) in
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
expandFrees	:: Map Name [RType] -> (RType, [(Name, Set RType)]) ->
			[(RType, [(Name, Set RType)])]
expandFrees reqs (RFree a, rest)
	= let possible	= findWithDefault [] a reqs in
		zip possible $ repeat rest
expandFrees reqs (RApplied bt at, rest)	-- the argument type (at) is always a free
	= expandFrees reqs (bt, rest) |> swap ||>> (\bt -> RApplied bt at) |> swap
expandFrees _ _	= []

{-
Makes the super type table complete, by recursively adding the supertypes of (known) super types.
-}
--expand	:: Map TypeID FullSuperTypeTable -> Map TypeID FullSuperTypeTable
expand all
	= let	-- Each TypeId should initially be updated with it's supertypes
		initialTodo	= all	|> keys
					& M.filter (not . L.null)
					||>> getBaseTID
					|> catMaybes |> S.fromList
		ctx	= Ctx M.empty initialTodo all in
		stt $ snd $ runstate _e ctx


data Context	= Ctx	{ notify	:: DG TypeID	-- if a node gets updated, the nodes connected to it should get updated too
			, todo		:: DG TypeID	-- These should get checked, because connected nodes were updated
			, stt		:: Map TypeID FullSuperTypeTable
			}
type St	= State Context

_e	:: St ()
_e	= do	td	<- get' todo
		unless (null td) $ do
		let (tid, changedTids)	= M.findMin td
		modify (\ctx -> ctx {todo = M.delete tid td})
		return ()

-- TODO

{- We know that any Type passed in changed is a super type of tid.
We take its super type table, and add all of those

-}
_expandOne	:: TypeID -> Set TypeID -> St ()
_expandOne tid changed
	= return ()



-- Marks Tid as changed
notifychanged	:: TypeID -> St ()
notifychanged tid
	= do	toChange	<- get' notify |> nodesFrom tid |> S.toList
		let newVertexes	= [(tid, n) | n <- toChange]
		td	<- get' todo |> addVertexes newVertexes
		modify (\ctx -> ctx {todo = td})
