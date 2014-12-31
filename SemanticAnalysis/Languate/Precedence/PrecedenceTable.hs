module Languate.Precedence.PrecedenceTable where

{--
This module implements the precedence table, a data structure which keeps track what associativity and precedence operators have.
--}

import StdDef
import Exceptions
import Data.Map hiding (map, foldr, null)
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


import Control.Arrow


type Operator	= Name
data PrecedenceTable	= PrecedenceTable { maxI::Int, op2i :: Map Operator Int, i2op :: Map Int [Operator], op2precMod :: Map Operator PrecModifier }

instance Show PrecedenceTable where
	show (PrecedenceTable _ op2i i2op mods)
		= precTable2md op2i i2op mods

modeOf	:: Int -> PrecedenceTable -> PrecModifier
modeOf index (PrecedenceTable _ _ i2op mods)
	= fromMaybe PrecLeft $ do	repr	<- lookup index i2op
					lookup (head repr) mods

precedenceOf	:: Expression -> PrecedenceTable -> Int
precedenceOf expr (PrecedenceTable tot op2i _ _)
		= if isOperator expr
			then	let (Operator nm)	= expr in
				findWithDefault (tot+1) nm op2i
			else	tot+2	-- plus 2, because normal expressions have a precedence lower then unknown operators (tot+1)


buildPrecTable	= stack' (indent' "Building Precedence table: ") . buildPrecTable'

buildPrecTable' 	:: World -> Exceptions String String PrecedenceTable
buildPrecTable' world
		= do	let mods		= elems $ modules world
			let (nameMod, rels)	= mergeTwo $ map getPrecedenceInfo mods
		  	let eqs			= eqRelations rels
			let ltRels		= ltRelations rels
			let allOps		= nub $ map fst nameMod ++ concatMap (\(n1,n2) -> [n1,n2]) (eqs ++ ltRels)
			-- all operators (''allOps'') are added in the end. This means that each operator has an representative, and each operator points to it's representative in the map
			let equivUnion		= union' (eqs ++ map (\n -> (n,n)) allOps)
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

-- checks that a class is consistent, thus that it contains not both left and right operators
-- checkNoMix	:: PrecedenceTable -> PrecedenceTable
checkNoMix	:: PrecedenceTable -> Exceptions String String ()
checkNoMix table
		= mapM_ (checkClass table) [1..maxI table]


checkClass	:: PrecedenceTable -> Int -> Exceptions String String ()
checkClass (PrecedenceTable maxI _ i2op modifs) i
		= do	haltIf (i > maxI) $ "Trying to check operator category "++show i++" on consistency, but only "++show maxI++" operator categories exists. This is a bug"
			haltIf (i <= 0) $ "Trying to check operator category "++show i++" on consistency, but categories start numbering from 1 (and not 0). This is a bug"
			let allOps	= findWithDefault [] i i2op
			let opsWithMod	= map (\op -> (op, findWithDefault PrecLeft op modifs)) allOps
			let mods	= nub $ map snd opsWithMod
			let errMods	= merge $ map swap opsWithMod
			let msg		= "Class "++show i++" is not consistent, there is a mixed precedence mode.\n"++
						"The "++show (length mods)++" found modes are: "++show mods++" with following operators: \n"++show errMods
			assert (1 == length mods) msg



errorMsg	:: [(Name, Name)] -> String
errorMsg faults	=  "Error: some operators have conflicting precedence relations: \n"
			++ foldr (\ops acc ->  acc ++ errorMsg' ops ) "\n" faults

errorMsg' (o1,o2)	= "\tBoth ("++o1++") = ("++o2++") and ("++o1++") < ("++o2++") are defined.\n"


mergeTwo	:: [([a],[b])] -> ([a],[b])
mergeTwo []	= ([],[])
mergeTwo ((as,bs):rest)
		= let (as', bs')	= mergeTwo rest in
		  	(as++as',bs++bs')
