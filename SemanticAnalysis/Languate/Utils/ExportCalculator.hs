module Languate.Utils.ExportCalculator where

{--
This module implements calculateExports and calculateImports
--}

import qualified Data.Map as M
import qualified Data.Set as S
import Prelude hiding (lookup)
import Data.Map (Map, lookup, insert)
import Data.Set (Set, unions, union)
import Data.Maybe
import State
import StdDef (merge, unmerge)
import Data.Tuple
import Control.Arrow

import Debug.Trace

{- calculates what properties each node exports, even by reexporting.
Params:
- import graph: node n imports all things from the given nodes
- export graph: inverse relation, node n is imported by given set
- local exports: function which gives, for a node, what properties it exports
- filter: for a given property, does this node re-export this property?
Returns:
- What properties each node exports
-}
calculateExports	:: (Eq prop, Ord n, Ord prop) => Map n (Set n) -> Map n (Set n) -> (n -> Set prop) -> (prop -> n -> Bool) -> Map n (Set prop)
calculateExports importGraph exportGraph exps impFilt
			= exported $ snd $ runstate _ce $ ExpS importGraph exportGraph exps impFilt M.empty (S.fromList $ M.keys importGraph)


invertDict		:: (Eq b, Ord b, Ord a) => Map a (Set b) -> Map b (Set a)
invertDict		= fmap S.fromList . M.fromList . merge . map swap . unmerge . M.toList . fmap S.toList

-- a imports from b, c, d
exampleImpGraph		= M.fromList $ map (second S.fromList) [("a", ["b","c"]),("b",[]),("c",[]),("d",["a"])]
exampleExps		= M.fromList $ map (second S.fromList) [("a",[1,2]),("b",[3]), ("c",[4]), ("d", [5])]


data ExpS n prop
		= ExpS 	{ importGraph :: Map n (Set n)	-- node --> imports from
			, exportGraph :: Map n (Set n)  -- node --> exports to
			, exps :: n -> Set prop
			, expFilter :: prop -> n -> Bool
			, exported :: Map n (Set prop)
			, worklist :: Set n}

type St n prop a	= State (ExpS n prop) a

_ce		:: (Eq prop, Ord n, Ord prop) => St n prop ()
_ce		=  do 	done	<- fmap not stillWork
			if done then
				return ()
			else do	(Just n)	<- pop
				possiblyChanged	<- rework n
				mapM_ push $ S.toList possiblyChanged
				_ce

-- reworks node n. Recalculates what node n would export. If the exports of node n have changed -> update it -> say which nodes should be reworked later
rework		:: (Ord n, Eq prop, Ord prop) => n -> St n prop (Set n)
rework n	=  do	localExps	<- getf exps n
			imps		<- fmap ((??) . lookup n) $ get' importGraph
			currentExps	<- get' exported
			let imported	=  unions $ catMaybes $ fmap (`lookup` currentExps) $ S.toList imps
			filtr		<- get' expFilter
			-- what n should export, according to the updated environment
			let exported	=  S.filter (`filtr` n) $ union localExps imported
			-- what n exports according to the current state
			let currNexps	= (??) $ lookup n currentExps
			if currNexps == exported then
				return S.empty	-- no new exports -> no new work!
			else	setExports exported n


setExports	:: Ord n => Set prop -> n -> St n prop (Set n)
setExports ps n	=  do	modify $ modExported $ M.insert n ps
			fmap ((??) . lookup n) $ get' exportGraph





pop	:: Ord n => St n prop (Maybe n)
pop	=  do	todo	<- get' worklist
		if S.null todo then return Nothing
			else do	let n	= S.findMin todo
				modify $ modWorkList $ S.delete n
				return $ Just n

push	:: Ord n => n -> St n prop ()
push n	=  modify $ modWorkList (S.insert n)

-- False: we're done!
stillWork	:: St n prop Bool
stillWork	=  do	todo	<- get' worklist
			return $ not $ S.null todo

setWorkList	:: Set n -> ExpS n prop -> ExpS n prop
setWorkList wl (ExpS importGraph expGraph exps reExpFilter exported _)
		=  ExpS importGraph expGraph exps reExpFilter exported wl

modWorkList	:: (Set n -> Set n) -> ExpS n prop -> ExpS n prop
modWorkList f s	=  setWorkList (f $ worklist s) s

setExported	:: Map n (Set prop) -> ExpS n prop -> ExpS n prop
setExported exported (ExpS importGraph expGraph exps reExpFilter _ wl)
		=  ExpS importGraph expGraph exps reExpFilter exported wl

modExported	:: (Map n (Set prop) -> Map n (Set prop)) -> ExpS n prop -> ExpS n prop
modExported f s	=  setExported (f $ exported s) s

getf		:: (s -> (a -> b)) -> a -> State s b
getf sf	a	=  do	f	<- get' sf
			return $ f a

(??)		:: Maybe (Set a) -> Set a
(??) m		=  fromMaybe S.empty m
