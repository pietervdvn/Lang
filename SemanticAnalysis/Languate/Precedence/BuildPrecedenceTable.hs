module Languate.Precedence.BuildPrecedenceTable where

{--
This module implements the actual building of the precedence table
--}

import StdDef
import Exceptions
import Data.Map hiding (map, foldr, null)
import qualified Data.Map as M
import Prelude hiding (lookup)
import Data.List hiding (lookup)
import Data.Maybe
import Data.Tuple (swap)
import Languate.FQN
import Languate.AST
import Languate.World

import Languate.Precedence.AnnotationStrip
import Languate.Precedence.BuildPrecTable
import Languate.Precedence.PrecTable2MD
import Languate.Precedence.CheckPrecStatements
import Languate.Precedence.PrecedenceTable

import Languate.Graphs.UnionFind

import Control.Arrow

buildPrecTable	= stack' (indent' "Building Precedence table: ") . buildPrecTable'

buildPrecTable' 	:: World -> Exceptions String String PrecedenceTable
buildPrecTable' world
		= do	let mods		= elems $ modules world
			mapM (uncurry checkPrecStmsIn) $ M.toList $ modules world
			let (nameMod, rels)	= mergeTwo $ map getPrecedenceInfo mods
		  	let eqs			= eqRelations rels
			let ltRels		= ltRelations rels
			let allOps		= nub $ map fst nameMod ++ concatMap (\(n1,n2) -> [n1,n2]) (eqs ++ ltRels)
			-- all operators (''allOps'') are added in the end. This means that each operator has an representative, and each operator points to it's representative in the map
			let equivUnion		= unionFind (eqs ++ map (\n -> (n,n)) allOps)
			let faultyLT		= checkIllegalLT ltRels equivUnion	-- erroronous LTs
			assert (null faultyLT) $ errorMsg faultyLT
			let partialOrder	= ltGraph $ withRepr equivUnion ltRels
			let totalOrder		= buildOrdering partialOrder
			let (op2i, i2op)	= second (fmap nub) $ buildTable totalOrder allOps equivUnion
		   	let table = PrecedenceTable (length totalOrder) op2i i2op (fromList nameMod)
			checkNoMix table
			return table


withRepr	:: Map Name Name -> [(Name, Name)] -> [(Name, Name)]
withRepr unions = map (repr *** repr)
			where repr n	= fromMaybe (error $ "BUG: The operator in an LT-relation is not defined: "++n) $ lookup n unions



mergeTwo	:: [([a],[b])] -> ([a],[b])
mergeTwo []	= ([],[])
mergeTwo ((as,bs):rest)
		= let (as', bs')	= mergeTwo rest in
		  	(as++as',bs++bs')
