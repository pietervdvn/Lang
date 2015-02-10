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
expand all
	= let	-- Each TypeId should initially be updated with it's supertypes
		initialTodo	= all	|> keys
					& M.filter (not . L.null)
					||>> getBaseTID
					|> catMaybes |> S.fromList
		initialSstt	= all	|> buildSpareSuperTypeTable
		-- updated node is empty at start, as it gets built while going
		ctx	= Ctx M.empty initialTodo all initialSstt in
		stt $ snd $ runstate _e ctx


data Context	= Ctx	{ notify	:: DG TypeID	-- if a node gets updated, the nodes connected to it should get updated too
			, todo		:: DG TypeID	-- These should get checked, because connected nodes were updated
			, stt		:: Map TypeID FullSuperTypeTable
			, sparestt		:: Map TypeID SpareSuperTypeTable
			}
type St	= State Context

_e	:: St ()
_e	= do	td	<- get' todo
		unless (null td) $ do
		let (tid, changedTids)	= M.findMin td
		modify (\ctx -> ctx {todo = M.delete tid td})
		_expandOne tid changedTids
		_e

{- We know that any Type passed in changed is a super type of tid.
We take its super type table, and add all of those
-}
_expandOne	:: TypeID -> Set TypeID -> St ()
_expandOne tid changed
	= do	let supers	= S.toList changed
		somethingChanged	<- mapM (_expandOne' tid) supers |> or
		when somethingChanged $ notifychanged tid

-- The bool indicates changes. Add the superTypeTable of superT to the stt of tid
_expandOne'	:: TypeID -> TypeID -> St Bool
_expandOne' tid superT
	= do	superSTT	<- get' stt |> findWithDefault M.empty superT |> M.toList
		mapM (_addSuper tid $ asRType' superT) superSTT |> or



_addSuper	:: TypeID -> RType ->  (RType, ([(Name, Set RType)], Binding, Maybe RType))
			-> St Bool
_addSuper base via (super, (reqs, binding, _))
	= _addSuper' base via super reqs binding



-- Returns a new FSTT if fstt has been changed
_addSuper'	:: TypeID -> RType -> RType -> [(Name, Set RType)] -> Binding
			-> St Bool
_addSuper' tid via super reqs newBind
	= do	fstt	<- get' stt |> findWithDefault M.empty tid
		sstt	<- get' sparestt |> findWithDefault M.empty tid
		let unifiable	= keys fstt |> unificate super & rights
		{- Assume: we can unify the super type against a already existing key.
		   This means that the super type already exists exactly (% free names)
			in the super type table, thus we do not have to do anything!
		-}
		let oldBind'	= _oldBinding fstt sstt super
		let oldBind	= fromMaybe noBinding oldBind'
		let super'	= substitute newBind super
		if not $ L.null unifiable then return False	-- nothing changed
		else do	let fstt'	= M.insert super' (reqs, newBind, Just via) fstt
			setSTTFor tid fstt'
			return True

-- Searches the binding which should be applied on super, the "old binding"
_oldBinding	:: FullSuperTypeTable -> SpareSuperTypeTable -> RType -> Maybe Binding
_oldBinding fstt sstt super
	= do	tid	<- getBaseTID super
		poss	<- M.lookup tid sstt
		Nothing


-- Marks Tid as changed
notifychanged	:: TypeID -> St ()
notifychanged tid
	= do	toChange	<- get' notify |> nodesFrom tid |> S.toList
		let newLinks	= [(tid, n) | n <- toChange]
		td	<- get' todo |> addLinks newLinks
		modify (\ctx -> ctx {todo = td})


notifyIfChanged	:: TypeID -> TypeID -> St ()
notifyIfChanged tidToNotify tid
	= do	graph	<- get' notify |> addLink (tid, tidToNotify)
		modify (\ctx -> ctx {notify = graph})


setSTTFor	:: TypeID -> FullSuperTypeTable -> St ()
setSTTFor tid stt'
	= do	ctx	<- get
		let ctx'	= ctx {stt = M.insert tid stt' $ stt ctx}
		put ctx'




buildSpareSuperTypeTable	:: FullSuperTypeTable -> SpareSuperTypeTable
buildSpareSuperTypeTable dict
	= dict & keys |> (\a -> (a,a)) ||>> getBaseTID
		|> swap |> unpackMaybeTuple & catMaybes	-- removing failed lookups
		& merge & M.fromList
