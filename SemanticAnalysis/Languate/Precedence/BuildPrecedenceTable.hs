module Languate.Precedence.BuildPrecedenceTable where

{--
This module implements the actual building of the precedence table
--}

import StdDef
import Exceptions
import Data.Map hiding (map, foldr, null, filter)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

import Prelude hiding (lookup)
import Data.List hiding (lookup)
import Data.Maybe
import Data.Tuple (swap)
import Languate.FQN
import Languate.AST
import Languate.Package

import Languate.Precedence.Utils
import Languate.Precedence.PrecTable2MD
import Languate.Precedence.CheckPrecStatements
import Languate.Precedence.PrecedenceTable

import Graphs.UnionFind
import Graphs.Order
import Graphs.DirectedGraph as DG

import Control.Arrow

import State

buildPrecTable	= stack' (indent' "Building Precedence table: ") . buildPrecTable'

buildPrecTable' 	:: Package -> Exceptions String String PrecedenceTable
buildPrecTable' package
		= do	let mods		= elems $ modules package
			mapM_ (uncurry checkPrecStmsIn) $ M.toList $ modules package
			let (nameMod, rels)	= mergeTwo $ map getPrecedenceInfo mods
		  	let eqs			= eqRelations rels
			let ltRels		= ltRelations rels
			let allOps		= nub ((rels >>= opsIn') ++ (nameMod |> fst))
			-- all operators (''allOps'') are added in the end. This means that each operator has an representative, and each operator points to it's representative in the map
			let equivUnion		= unionFind (eqs ++ map (\n -> (n,n)) allOps)
			let faultyLT	= searchIllegalLT ltRels equivUnion
			assert (null faultyLT) $ errorMsg faultyLT
			-- graph representing ("*" should be done before "-","+")
			let lowerThen	= addLinks (withRepr equivUnion ltRels) DG.empty
			let totalOrder'		= buildOrdering lowerThen
			totalOrder <- case totalOrder' of
					Left loops	-> halt $ "Could not build the precedence table, as there is a loop in the precedences: "++ unwords (loops |> show)
					Right ordering	-> return ordering
			let (op2i, i2op)	= buildTable totalOrder allOps equivUnion
		   	let table = PrecedenceTable (length totalOrder) op2i i2op (fromList nameMod)
			checkNoMix table
			return table


---------------------------
-- Building of i2op/op2i --
---------------------------

-- actual construction of the "PrecedenceTable"
buildTable	:: [Name] -> [Name] -> Map Name Name -> (Map Name Int, Map Int (Set Name))
buildTable repres allOps unions
		=  let  repres'	= fromList $ zip repres [0..]	-- each representative gets its number
			defaul	= length repres
			allOps'	= allOps |> (\n -> n `lookup` unions >>= (`lookup` repres')) |> fromMaybe defaul	-- each op gets its number
			op2i	= fromList $ zip allOps allOps'
			i2op	= fmap S.fromList . fromList . merge $ zip allOps' allOps in
			(op2i, i2op)






{-
Checks if a ''(+) < (-)'' relation does not exist if ''(+) = (-)'' is defined somewhere. This checks that -if two operators are in the same union- no LT-relation exists between them.
Returns the faulty arguments. -}
searchIllegalLT	:: [(Name, Name)] -> Map Name Name -> [(Name,Name)]
searchIllegalLT ltRels repres
		=  let canons	= map (canonLT repres) ltRels in
		   let zipped	= zip ltRels canons in
			filter (isSame . snd) zipped |> fst
			where isSame (o1,o2)	= o1 == o2



-------------------------------
-- WRITING AS REPRESENTATIVE --
-------------------------------

-- writes the tuple as their representative
canonLT		:: Map Name Name -> (Name,Name) -> (Name, Name)
canonLT dict (o1, o2)
		=  fromJust $ do	r1	<- lookup o1 dict
					r2	<- lookup o2 dict
					return (r1,r2)


withRepr	:: Map Name Name -> [(Name, Name)] -> [(Name, Name)]
withRepr unions = map (repr *** repr)
			where repr n	= findWithDefault (error $ "BUG: The operator '"++n++"' has no representative") n unions




mergeTwo	:: [([a],[b])] -> ([a],[b])
mergeTwo []	= ([],[])
mergeTwo ((as,bs):rest)
		= let (as', bs')	= mergeTwo rest in
		  	(as++as',bs++bs')
