module Languate.Graphs.DirectedGraph (DG, invert, addVertex, nodesFrom, leafs, dropNodes, isEmpty) where

import StdDef ((||>>),(|>))

import Data.Map as M
import Data.Set as S
import Data.List as L (filter)
import Data.Tuple (swap)



type DirectedGraph n	= Map n (Set n)
type DG n	= DirectedGraph n

type Inverted n	= DG n

-- Inverts a directed graph
invert	:: (Ord n, Eq n) => DG n -> DG n
invert 	= fmap S.fromList . M.fromList . merge . fmap swap . unmerge . M.toList . fmap S.toList


addVertex	:: (Ord n, Eq n) => (n,n) -> DG n -> DG n
addVertex (from, to) graph
	= let newVertexes	= S.insert to $ nodesFrom from graph in
		M.insert from newVertexes graph

-- Nodes reachable from n with one hop
nodesFrom	:: (Ord n, Eq n) => n -> DG n -> Set n
nodesFrom	= findWithDefault S.empty


-- Gives all the leafs of the graph (nodes with no outgoing vertexes)
leafs	:: (Ord n, Eq n) => DG n -> Set n
leafs graph
	= S.fromList . fmap fst . L.filter (S.null . snd) $ M.toList graph

dropNodes	:: (Ord n, Eq n) => Set n -> DG n -> DG n
dropNodes ns graph
	= S.foldr M.delete graph ns |> (S.filter (`S.notMember` ns))

merge		:: Eq a => [(a,b)] -> [(a,[b])]
merge []	= []
merge ((a,b):ls)
		= let bs	= fmap snd $ L.filter ((==) a . fst) ls in
			(a,b:bs): merge (L.filter ((/=) a . fst) ls)

unmerge		:: [(a,[b])] -> [(a,b)]
unmerge 	=  concatMap (\(a,bs) -> [(a,b) | b <- bs])

isEmpty	:: DG n -> Bool
isEmpty	= M.null
