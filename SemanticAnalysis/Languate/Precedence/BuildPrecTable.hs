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
import Prelude hiding (lookup)
import Data.List (nub)

import Control.Arrow
import Data.Tuple

import Languate.Graphs.Order

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


-- actual construction of a "PrecedenceTable"-datastructure is done in 'PrecedenceTable.hs'. This here does the heavy lifting.
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



ltGraph rels	=  let allOps	= rels >>= (\(n1,n2) -> [n1,n2]) in
		   let startDict	= fromList $ zip allOps (repeat []) in
			snd $ runstate (mapM_ collectGT rels) (startDict, startDict)

{- collect gt adds the right argument to the list of the first. This way, we can see immediatly which has a class that is bigger
It does the reverse for the second map
-}
collectGT	:: (Name, Name) -> State (Map Name [Name], Map Name [Name]) ()
collectGT (o1,o2)
		=  do	modify $ first $ insertLst o1 o2
			modify $ second $ insertLst o2 o1
