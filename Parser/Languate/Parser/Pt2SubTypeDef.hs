module Languate.Parser.Pt2SubTypeDef (pt2subdef) where

import StdDef
import Bnf.ParseTree
import Bnf
import Languate.Parser.Utils
import Languate.Parser.Pt2Type
import Languate.Parser.Pt2DataDef

import Languate.AST

{--
This module converts a parse tree into a subtype declaration
--}

-- ## GEN (typeExpr)


modName	= "Pt2SubDef"

pt2subdef	:: ParseTree -> SubDef
pt2subdef	=  pt2a h t s convert

convert		:: AST -> SubDef
convert (SubDefT sd)
		=  sd
convert ast	=  convErr modName ast

data AST	= Ident Name
		| FreeTypes [Name] [TypeRequirement]
		| EqualT	| SubTypeT
		| PrivT		| AndT
		| Type Type [TypeRequirement]
		| SubDefT SubDef
	deriving (Show)


h		:: [(Name, ParseTree -> AST)]
h		=  [("freeTypes",uncurry FreeTypes . pt2freetypes),("type", uncurry Type . pt2type)]

t		:: Name -> String -> AST
t "typeIdent" nm
		= Ident nm
t _ "subtype"	= SubTypeT
t _ "="		= EqualT
t _ "_"		= PrivT
t nm cont	= tokenErr modName nm cont


s		:: Name -> [AST] -> AST
s _ [SubTypeT, Ident nm, EqualT, Type t reqs]
		= SubDefT $ SubDef nm Public [] (topLevelConj t) reqs
s r (SubTypeT: Ident nm: FreeTypes frees reqs:tail)
		= let (SubDefT (SubDef name vis _ supers reqs')) = s r (SubTypeT:Ident nm:tail) in
			SubDefT $ SubDef name vis frees supers (reqs ++ reqs')
s r (SubTypeT:PrivT:tail)
		= let (SubDefT (SubDef nm _ frees supers reqs)) = s r (SubTypeT:tail) in
			SubDefT $ SubDef nm Private frees supers reqs
s _ [ast]	= ast
s nm asts	= seqErr modName nm asts
