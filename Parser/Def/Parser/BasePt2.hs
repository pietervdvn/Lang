module Def.Parser.Pt2Function (pt2func) where

import StdDef
import Bnf.ParseTree
import Bnf
import Def.Parser.Utils
import Def.AST

{--

This module converts the ParseTree into a function declaration, with laws etc.
Declarations may have multiple (explicit) types

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




