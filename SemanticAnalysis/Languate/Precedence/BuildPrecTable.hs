module Languate.Precedence.BuildPrecTable where

{--
This module builds the precedence table, step by step.
--}

import StdDef
import State
import Languate.AST

import Data.Map hiding (union, map, filter, null, foldr)
import qualified Data.Map as Map
import Data.Maybe
import Data.List (nub)
import Prelude hiding (lookup)

import Control.Arrow
import Data.Tuple

import Debug.Trace

{- ### Checks
Error msgs are generated somewhere else
-}
{- checks if a ''(+) < (-)'' relation does not exist if ''(+) = (-)'' is defined somewhere. Thus, if two operators are in the same union, checks no LT-relation exists between them.
Returns faulty arguments.
-}
checkIllegalLT	:: [(Name, Name)] -> Map Name Name -> [(Name,Name)]
checkIllegalLT ltRels dict
		=  let canons	= map (canonLT dict) ltRels in
		   let zipped	= zip ltRels canons in
			map fst $ filter (isSame . snd) zipped
			where isSame (o1,o2)	= o1 == o2


-- ### Building of actual table


buildTable	:: [Name] -> [Name] -> Map Name Name -> (Map Name Int, Map Int [Name])
buildTable repres allOps unions
		=  let repres'	= zip repres [1..] in
		   let prec 	= (fromList repres', fromList $ map (\(o,i) -> (i,[o])) repres') in
			foldr (\op acc -> addOp op unions acc) prec allOps


addOp		:: Name -> Map Name Name -> (Map Name Int, Map Int [Name]) -> (Map Name Int, Map Int [Name])
addOp op unions	(dict, dict')
		=  let	repres	= fromJust $ lookup op unions in
		   let i	= fromJust $ lookup repres dict in
			(insert op i dict, insertLst i op dict')


-- ### Building of partial and ordering

-- gets a list of ("a" --> ["b","c"]), meaning "a" should be evaluated before b and c, and the inverse relation.
-- Gives a partion ordering by removing representative operations step by step.
buildOrdering	:: (Map Name [Name], Map Name [Name]) -> [Name]
buildOrdering rels@(ltRel, revRel)
		=  	let highestPreced	= emptyKey ltRel in
			if null highestPreced then	checkCycle ltRel -- either we're done or are stuck on a loop.
				else 	let op 	= head highestPreced in
					op : buildOrdering (removeGroup op rels)

checkCycle	:: Map Name [Name] -> [Name]
checkCycle dict	=  if Map.null dict then [] else error $ "Precedence building: stuck in a loop! "++show dict

-- > {"a" --> ["b","c"],"b" --> ["c"], "c" --> []}, {"c" --> ["a","b"], "b" --> "a"}
-- The second relation in the tuple is the 'reverse relation'. This means we can use that to quickly shorten lists.
removeGroup	:: Name -> (Map Name [Name], Map Name [Name]) -> (Map Name [Name], Map Name [Name])
removeGroup o (ltRel, revRel)
		= let 	toRems	= fromMaybe [] $ lookup o revRel in
			(delete o $ foldr (deleteFromLst o) ltRel toRems, revRel)

deleteFromLst	:: Name -> Name -> Map Name [Name] -> Map Name [Name]
deleteFromLst n	=  adjust (filter (n /=))

-- searches for entries with empty keys
emptyKey	:: Map Name [Name] -> [Name]
emptyKey dict	=  map fst $ filter (null . snd) $ toList dict

-- assumes each element points to it's smallest representative
neededGroups	:: Map Name Name -> Int
neededGroups dict
		=  let reprs	= map snd $ toList dict in
			length $ nub reprs

-- writes the tuple as their representative
canonLT		:: Map Name Name -> (Name,Name) -> (Name, Name)
canonLT dict (o1, o2)
		=  fromJust $ do	r1	<- lookup o1 dict
					r2	<- lookup o2 dict
					return (r1,r2)

ltGraph		:: [(Name,Name)] -> (Map Name [Name], Map Name [Name])
ltGraph rels	=  let allOps	= concatMap (\(n1,n2) -> [n1,n2]) rels in
		   let startDict	= fromList $ zip allOps (repeat []) in
			snd $ runstate (mapM_ collectGT rels) (startDict, startDict)

{- collect gt adds the right argument to the list of the first. This way, we can see immediatly which has a class that is bigger
It does the reverse for the second map
-}
collectGT	:: (Name, Name) -> State (Map Name [Name], Map Name [Name]) ()
collectGT (o1,o2)
		=  do	modify $ first $ insertLst o1 o2
			modify $ second $ insertLst o2 o1


insertLst	:: Ord k => k -> v -> Map k [v] -> Map k [v]
insertLst k v dict
		=  case lookup k dict of
			Nothing	-> insert k [v] dict
			(Just lst)	-> insert k (v:lst) dict

-- ### Union find algorithm

union'	:: [(Name, Name)] -> Map Name Name
union' tuples
	= snd $ runstate (mapM_ add tuples) empty

-- the map is a map of pointers. A name will always try to point to its representative, being the smallest element of the set.
-- State invariant: if the name appears as value in the map, it will also appear as key.
add	:: (Name, Name) -> State (Map Name Name) ()
add (n1,n2)	= do	repres1	<- representative n1
			repres2	<- representative n2
			let r	= if repres1 < repres2 then repres1 else repres2
			modify $ insert n1 r
			modify $ insert n2 r
			modify $ insert repres1 r
			modify $ insert repres2 r


-- searches the representative for the given name. Does never insert nm in the map.
representative	:: Name -> State (Map Name Name) Name
representative nm
		= do 	dict	<- get
			let nmv	= lookup nm dict
			case nmv of
				(Just found)	-> collapse nm found
				(Nothing)	-> return nm


collapse	:: Name -> Name -> State (Map Name Name) Name
collapse nm found
	| nm == found	= return nm
	| otherwise	= do	repres	<- representative found
				modify	$ insert found repres
				return repres
