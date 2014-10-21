module Languate.Precedence.PrecedenceTable where

{--
This module implements the precedence table, a data structure which keeps track what associativity and precedence operators have.
--}

import StdDef
import Data.Map hiding (map, foldr, null)
import Prelude hiding (lookup)
import Data.List hiding (lookup)
import Data.Maybe
import Languate.FQN
import Languate.AST

import Languate.Precedence.AnnotationStrip
import Languate.Precedence.BuildPrecTable
import Languate.Precedence.PrecTable2MD


import Control.Arrow


type Operator	= Name
data PrecedenceTable	= PrecedenceTable Int (Map Operator Int) (Map Int [Operator]) (Map Operator PrecModifier)

instance Show PrecedenceTable where
	show (PrecedenceTable _ op2i i2op mods)
		= precTable2md op2i i2op mods

modeOf	:: Int -> PrecedenceTable -> PrecModifier
modeOf index (PrecedenceTable _ _ i2op mods)
	= fromJust $ do	repr	<- lookup index i2op
			lookup (head repr) mods

precedenceOf	:: Expression -> PrecedenceTable -> Int
precedenceOf expr (PrecedenceTable tot op2i _ _)
		= if isOperator expr
			then
				let (Operator nm)	= expr in
				fromJust $ lookup nm op2i
			else	tot+1


buildPrecTable modules
		=  let (nameMod, rels)	= mergeTwo $ map getPrecedenceInfo modules in
		   let eqs	= eqRelations rels in
		   let ltRels	= ltRelations rels in
		   let allOps	= nub $ map fst nameMod ++ concatMap (\(n1,n2) -> [n1,n2]) (eqs ++ ltRels) in
			-- all operators (''allOps'') are added in the end. This means that each operator has an representative, and each operator points to it's representative in the map
		   let equivUnion	= union' (eqs ++ map (\n -> (n,n)) allOps) in
		   let faultyLT		= checkIllegalLT ltRels equivUnion in
		   let equivUnion'	= if null faultyLT then equivUnion else error $ errorMsg faultyLT in
		   let partialOrder	= ltGraph $ withRepr equivUnion' ltRels in
		   let totalOrder	= buildOrdering partialOrder in
		   let (op2i, i2op)		= second (fmap nub) $ buildTable totalOrder allOps equivUnion' in
		   	checkNoMix $ PrecedenceTable (length totalOrder) op2i i2op (fromList nameMod)


withRepr	:: Map Name Name -> [(Name, Name)] -> [(Name, Name)]
withRepr unions = map (\(n1,n2) -> (repr n1, repr n2))
			where repr n	= fromMaybe (error $ "BUG: The operator in an LT-relation is not defined: "++n) $ lookup n unions

-- checks that a class is consistent, thus that it contains not both left and right operators
-- checkNoMix	:: PrecedenceTable -> PrecedenceTable
checkNoMix	:: PrecedenceTable -> PrecedenceTable
checkNoMix table@(PrecedenceTable _ _ i2op mods)
		= let voids	= mapWithKey checkClass $ fmap (map (\op -> fromJust $ lookup op mods)) i2op in
			if and $ map snd $ toList voids then table else error $ "Mixed precedence!"
			where checkClass i mods	= if 1 == (length $ nub mods) then True else error $ "Precedence table error: mixed associativity in class "++show i++", operators with associativity "++show mods++" exist."


errorMsg	:: [(Name, Name)] -> String
errorMsg faults	=  "Error: some operators have conflicting precedence relations: "
			++ foldr (\ops acc ->  acc ++ errorMsg' ops ) "\n" faults

errorMsg' (o1,o2)	= "\tBoth ("++o1++") = ("++o2++") and ("++o1++") < ("++o2++") are defined.\n"


mergeTwo	:: [([a],[b])] -> ([a],[b])
mergeTwo []	= ([],[])
mergeTwo ((as,bs):rest)
		= let (as', bs')	= mergeTwo rest in
		  	(as++as',bs++bs')
