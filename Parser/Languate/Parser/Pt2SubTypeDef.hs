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
		| TypeTail [Type] [TypeRequirement]
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
t _ "&"		= AndT
t nm cont	= tokenErr modName nm cont


s		:: Name -> [AST] -> AST
s _ [AndT, Type t tr]
		= TypeTail [t] tr
s r (Type t tr:tail)
		= let TypeTail ts trs	= s r tail in
			TypeTail (t:ts) (tr++trs)
s r (SubTypeT:Ident nm:EqualT:tail)
		= s r (SubTypeT:Ident nm:FreeTypes [] []:EqualT:tail)
s r (SubTypeT:Ident nm:FreeTypes frees tr:EqualT:PrivT:tail)
		= let SubDefT (SubDef nm' _ frees' ts' trs') = s r (SubTypeT:Ident nm:FreeTypes frees tr:EqualT:tail) in
			SubDefT $ SubDef nm' Private frees' ts' trs'
s r (SubTypeT:Ident nm:FreeTypes frees tr:EqualT:Type t tr0:[])
		= s r (SubTypeT:Ident nm:FreeTypes frees tr:EqualT:Type t tr0:TypeTail [] []:[])
s _ (SubTypeT:Ident nm:FreeTypes frees tr:EqualT:Type t tr0:TypeTail ts trs:[])
		= SubDefT $ SubDef nm Public frees (t:ts) (tr++tr0++trs)


s _ [ast]	= ast
s nm asts	= seqErr modName nm asts
