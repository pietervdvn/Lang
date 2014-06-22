module Def.Parser.Pt2Function (pt2func) where

import StdDef
import Bnf.ParseTree
import Bnf
import Def.Parser.Utils
import Def.AST

{--


Where it all comes together.

This module converts the ParseTree into a full 

--}

modName	= "Pt2Function"

pt2func	:: ParseTree -> 
pt2func	=  pt2a h t s convert . cleanAll ["nl"]

convert		:: AST -> Expression
convert ast	=  convErr modName ast


data AST	= Declaration Name Type
		| LawAst Law
		| Comm [Comment]
		| Line [Pattern] Expression
	deriving (Show)


h		:: [(Name, ParseTree -> AST)]
h		=  []

t		:: Name -> String -> AST
t nm cont	=  tokenErr modName nm cont


s		:: Name -> [AST] -> AST
s _ [ast]	= ast
s nm asts	= seqErr modName nm asts




