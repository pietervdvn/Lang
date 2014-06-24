module Languate.Parser.Pt2DataDef (pt2adtdef, pt2freetypes) where

import StdDef
import Bnf.ParseTree
import Bnf
import Languate.Parser.Utils
import Languate.Parser.Pt2Comment
import Languate.Parser.Pt2DataDefProd
import Languate.AST

{--

This module converts the ParseTree into a data def. The heavy lifting --parsing the constructors-- is done by pt2datadefprod
--}

modName	= "Pt2DataDef"

pt2adtdef	:: ParseTree -> ([Comment],ADTDef)
pt2adtdef	=  pt2a h t s convert

convert		:: AST -> ([Comment], ADTDef)
convert (Data comms vis name frees sums)
		=  (init' comms, ADTDef name frees (last comms) sums)
convert ast	=  convErr modName ast


data AST	= Comms [Comment]
		| Ident Name
		| FreeTypes [Name]
		| Prod [ADTSum]
		| Data [Comment] Visible Name [Name] [ADTSum]
		| DataT	| PrivT	| EqualT
	deriving (Show)


h		:: [(Name, ParseTree -> AST)]
h		=  [("nlcomments", Comms . pt2nlcomments),("prod", Prod . pt2prod),("freeTypes", FreeTypes . pt2freetypes)]

t		:: Name -> String -> AST
t "globalIdent" id
		=  Ident id
t "localIdent" id
		=  FreeTypes [id]
t _ "data"	=  DataT
t _ "_"		=  PrivT
t _ "="		=  EqualT
t nm cont	=  tokenErr modName nm cont


s		:: Name -> [AST] -> AST

s _ (Comms comms:DataT:PrivT:tail)
		= let Data comms' _ name freetypes sums = s "data" (Comms comms:DataT:tail) in
			Data comms' Private name freetypes sums
s _ (Comms comms:DataT:Ident name:FreeTypes frees:tail)
		=  let Data comms' vis name' _ sums = s "data" (Comms comms:DataT:Ident name:tail) in
			Data comms' vis name' frees sums
s _ [Comms comms, DataT, Ident name,EqualT, Prod sums]
		= Data comms Public name [] sums
s _ [ast]	= ast
s nm asts	= seqErr modName nm asts

pt2freetypes	:: ParseTree -> [Name]
pt2freetypes	=  pt2a [] (\"localIdent" id -> Id id) sf conv

sf		:: Name -> [ASTf] -> ASTf
sf _		=  Ids . map (\(Id id) -> id)

data ASTf	= Id Name
		| Ids [Name]

conv		:: ASTf -> [Name]
conv (Ids nms)	=  nms


