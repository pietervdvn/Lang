module Languate.Parser.Pt2TypeConj (pt2typeConj) where

import StdDef
import Bnf.ParseTree
import Bnf
import Languate.Parser.Utils
import Languate.Parser.Pt2Type
import Languate.AST hiding (Tuple, Seq)
{--
This module parses Type conjunctions
--}

-- Converts a parse tree of the form "Set (a:Ord) & Eq"  to (["Set a", "Eq"], {a --> Ord})
pt2typeConj :: ParseTree -> ([Type], [TypeRequirement])
pt2typeConj	= pt2a h t s convert

packName	= "Pt2TypeConj"

data AST	= TypeT Type [TypeRequirement]
		| ConjT
		| Seq [AST]
	deriving (Show, Eq)

convert		:: AST -> ([Type], [TypeRequirement])
convert	(Seq asts)
		=  let	(tps, treqs)	= unzip $ map convert asts in
			(concat tps, concat treqs)
convert (TypeT t treqs)
		= ([t], treqs)


h		:: [(Name, ParseTree -> AST)]
h		=  [("type", uncurry TypeT . pt2type)]


t		:: Name -> String -> AST
t "typeConjT" _	= ConjT
t nm cont	= tokenErr packName nm cont

s		:: Name -> [AST] -> AST
s _ [ConjT, ast]
		= ast
s _ [ast]  	= ast
s _ asts	= Seq asts
