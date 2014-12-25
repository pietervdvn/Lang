module Languate.ImportTable.ExportCalculator where

{--
This module implements calculateExports and calculateImports
--}
import StdDef
import qualified Data.Map as M
import qualified Data.Set as S
import Prelude hiding (lookup)
import Data.Map (Map, lookup, insert, findWithDefault, mapWithKey)
import Data.Set (Set, unions, union)
import Data.Maybe
import State
import StdDef (merge, unmerge)
import Data.Tuple
import Control.Arrow

import Languate.World hiding (importGraph)
import qualified Languate.World as W
import Languate.FQN

calculateExports'	:: (Ord prop, Eq prop) => World -> (FQN -> Set prop) -> (FQN -> (FQN, prop) -> Bool) -> Map FQN (Set prop)
calculateExports' w	= let ig	= W.importGraph w in
 				calculateExports ig (invertDict ig)

calculateImports'	:: (Ord prop, Eq prop) => World -> (FQN -> Set prop) -> Map FQN (Set prop) -> Map FQN (Set prop)
calculateImports' w	= calculateImports (W.importGraph w)

{- calculates what properties each node exports, even by reexporting.
Params:
- import graph: node n imports all things from the given nodes
- export graph: inverse relation, node n is imported by given set
- local exports: function which gives, for a node, what properties it exports. These are not passed through the secondary filter
- filter: for a given property, does this node re-export this property? (property, imported out of node) -> current node -> current node re-exports?
Returns:
- What properties each node exports.
-}
calculateExports	:: (Eq prop, Ord n, Ord prop) => Map n (Set n) -> Map n (Set n) -> (n -> Set prop) -> (n -> (n,prop) -> Bool) -> (Map n (Map n prop), Map
calculateExports importGraph exportGraph exps impFilt
			= exported $ snd $ runstate _ce $ ExpS importGraph exportGraph exps impFilt M.empty M.empty (S.fromList $ M.keys importGraph)

{- calculateImports gives all things which are visible within n (thus local -private- props and imported props)

-}
calculateImports	:: (Eq prop, Ord n, Ord prop) => (Map n (Set n)) -> (n -> Set prop) -> Map n (Set prop) -> Map n (Set prop)
calculateImports importGraph local exports
			= mapWithKey (\n _ -> calculateImportsFor importGraph local exports n) exports

calculateImportsFor	:: (Eq prop, Ord n, Ord prop) => (Map n (Set n)) -> (n -> Set prop) -> Map n (Set prop) -> n -> Set prop
calculateImportsFor importGraph local exports n
			=  let	importsFrom	= S.toList $ (??) $ lookup n importGraph
				imported	= fmap (\n -> findWithDefault S.empty n exports) importsFrom
				localDecl	= local n in
				union localDecl $ unions imported




data ExpS n prop
		= ExpS 	{ importGraph :: Map n (Set n)	-- node --> imports from
			, exportGraph :: Map n (Set n)  -- node --> exports to
			, exps :: n -> Set prop		-- base exports
			, expFilter :: n -> (n,prop) -> Bool	-- (property, imported via n) -> current n -> current n re-exports prop?
			, exported :: Map n (Set prop)	-- currently, node n --> exports these props
			-- property prop is imported into node n via one (or more) other nodes
			, importedVia	:: Map n (Set (prop,n))
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
rework n	=  do	-- The nodes which n imports
			imps	<- get' importGraph |> lookup' n
			-- what each node exports for the moment : Map n (Set prop)
			curExps	<- get' exported
			-- what each node exports for the moment and is visible in n	:: Set (n, Set prop)
			let impProps	= S.map (\n -> (n, lookup' n curExps)) imps
			let impProps'	= setUnions $ S.map (\(n, props) -> S.map (\p -> (n, p)) props) impProps	-- :: Set  (prop, n)
			-- filter function. Decides wether a imported property passes or not
			filtr		<- get' expFilter |> (\filtr -> filtr n)
			-- All currently reexported stuff!
			let reexported	= S.filter filtr impProps'	-- :: Set (prop,n)
			let impVia	= S.map (\(via, prop) -> (n, (prop,via))) reexported
			addImportedVia impVia
			let reexported'	= S.map snd reexported
			localExported	<- get' exps |> (\f -> f n)		-- Set prop
			let newExports	= union localExported reexported'
			let oldExports	= lookup' n curExps
			if newExports == oldExports then return S.empty	-- no changes! hooray
				else setExports newExports n

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
setWorkList wl (ExpS importGraph expGraph exps reExpFilter exported impVia _)
		=  ExpS importGraph expGraph exps reExpFilter exported impVia wl

modWorkList	:: (Set n -> Set n) -> ExpS n prop -> ExpS n prop
modWorkList f s	=  setWorkList (f $ worklist s) s

setExported	:: Map n (Set prop) -> ExpS n prop -> ExpS n prop
setExported exported (ExpS importGraph expGraph exps reExpFilter _ impVia wl)
		=  ExpS importGraph expGraph exps reExpFilter exported impVia wl

setImportedVia	:: Map n (Set (prop,n)) -> ExpS n prop -> ExpS n prop
setImportedVia impVia (ExpS importGraph expGraph exps reExpFilter exported _ wl)
		=  ExpS importGraph expGraph exps reExpFilter exported impVia wl


modExported	:: (Map n (Set prop) -> Map n (Set prop)) -> ExpS n prop -> ExpS n prop
modExported f s	=  setExported (f $ exported s) s

getf		:: (s -> (a -> b)) -> a -> State s b
getf sf	a	=  do	f	<- get' sf
			return $ f a

addImportedVia	:: (Ord n, Ord prop) => Set (n, (prop,n)) -> St n prop ()
addImportedVia s
		= do 	dict		<- get' importedVia
			let merged 	= S.foldl (\acc (n, p) -> M.insert n (S.insert p $ lookup' n acc) acc) dict s
			modify $ setImportedVia merged

setUnions	:: (Ord a) => Set (Set a) -> Set a
setUnions sets	=  S.foldl (\acc set -> S.union set acc) S.empty sets

(??)		:: Maybe (Set a) -> Set a
(??) m		=  fromMaybe S.empty m

lookup'		:: (Ord k) => k -> Map k (Set v) -> Set v
lookup' k dict	=  (??) $ lookup k dict
