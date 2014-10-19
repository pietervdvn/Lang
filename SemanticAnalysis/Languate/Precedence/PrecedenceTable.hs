module Languate.Precedence.PrecedenceTable where

{--
This module implements the precedence table, a data structure which keeps track what associativity and precedence operators have.
--}

import StdDef
import Data.Map hiding (map, foldr, null)
import Languate.FQN
import Languate.AST

import Languate.Precedence.AnnotationStrip
import Languate.Precedence.BuildPrecTable

buildPrecTable	:: [Module] -> Map Name Name
buildPrecTable modules
		=  let (nameMod, rels)	= mergeTwo $ map getPrecedenceInfo modules in
		   let allOps	= map fst nameMod in
		   let eqs	= eqRelations rels in
		   let ltRels	= ltRelations rels in
			-- all operators (''allOps'') are added in the end. This means that each operator has an representative, and each operator points to it's representative in the map
		   let equivUnion	= union' (eqs ++ map (\n -> (n,n)) allOps) in
		   let faultyLT		= checkIllegalLT ltRels equivUnion in
			if null faultyLT then equivUnion
				else error $ errorMsg faultyLT


errorMsg	:: [(Name, Name)] -> String
errorMsg faults	=  "Error: some operators have conflicting precedence relations: "
			++ foldr (\ops acc ->  acc ++ errorMsg' ops ) "\n" faults

errorMsg' (o1,o2)	= "\tBoth ("++o1++") = ("++o2++") and ("++o1++") < ("++o2++") are defined.\n"


mergeTwo	:: [([a],[b])] -> ([a],[b])
mergeTwo []	= ([],[])
mergeTwo ((as,bs):rest)
		= let (as', bs')	= mergeTwo rest in
		  	(as++as',bs++bs')
