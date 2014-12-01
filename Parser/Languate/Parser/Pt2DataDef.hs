module Languate.Parser.Pt2DataDef (pt2adtdef, pt2freetypes) where

import StdDef
import Bnf.ParseTree
import Bnf
import Languate.Parser.Utils
import Languate.Parser.Pt2Comment
import Languate.Parser.Pt2DataDefProd
import Languate.Parser.Pt2Type
import Languate.AST
import Data.List (nub)

{--

This module converts the ParseTree into a data def. The heavy lifting --parsing the constructors-- is done by pt2datadefprod
--}

modName	= "Pt2DataDef"

pt2adtdef	:: ParseTree -> ([Comment],ADTDef)
pt2adtdef	=  pt2a h t s convert

convert		:: AST -> ([Comment], ADTDef)
convert (Data comms vis name frees sums reqs)
		=  let edit	= if vis == Private then map (setVisibility vis) else id in
			(init' comms, ADTDef name frees reqs (last comms) $ edit sums)
convert ast	=  convErr modName ast


data AST	= Comms [Comment]
		| Ident Name
		| FreeTypes [Name] [TypeRequirement]
		| Prod [ADTSum] [TypeRequirement]
		| Data [Comment] Visible Name [Name] [ADTSum] [TypeRequirement]
		| DataT	| PrivT	| EqualT
	deriving (Show)


h		:: [(Name, ParseTree -> AST)]
h		=  [("nlcomments", Comms . pt2nlcomments),("prod", uncurry Prod . pt2prod),("freeTypes", uncurry FreeTypes . pt2freetypes)]

t		:: Name -> String -> AST
t "globalIdent" id
		=  Ident id
t "typeIdent" id
		=  Ident id
t "localIdent" id
		=  FreeTypes [id] []
t _ "data"	=  DataT
t _ "_"		=  PrivT
t _ "="		=  EqualT
t nm cont	=  tokenErr modName nm cont


s		:: Name -> [AST] -> AST

s _ (Comms comms:DataT:PrivT:tail)
		= let Data comms' _ name freetypes sums reqs = s "data" (Comms comms:DataT:tail) in
			Data comms' Private name freetypes sums reqs
s _ (Comms comms:DataT:Ident name:FreeTypes frees reqs:tail)
		=  let Data comms' vis name' _ sums reqs = s "data" (Comms comms:DataT:Ident name:tail) in
			Data comms' vis name' frees sums reqs
s _ [Comms comms, DataT, Ident name,EqualT, Prod sums reqs]
		= Data comms Public name [] sums reqs
s _ [ast]	= ast
s "data" (DataT:Ident name:_)
		= error $ "No comment for data definition "++show name
s "data" (DataT:PrivT:Ident name:_)
		= error $ "No comment for data definition "++show name
s nm asts	= seqErr modName nm asts

pt2freetypes	:: ParseTree -> ([Name],[TypeRequirement])
pt2freetypes	=  pt2a [("constrFreeType", uncurry Id . pt2type)] (tokenErr "No tokenize should be needed on freeTypes") sf conv

sf		:: Name -> [ASTf] -> ASTf
sf _		=  uncurry Ids . unzip . map (\(Id (Free id) reqs) -> (id,reqs))

data ASTf	= Id Type [TypeRequirement]
		| Ids [Name] [[TypeRequirement]]

conv		:: ASTf -> ([Name], [TypeRequirement])
conv (Ids nms reqs)	=  (nms,nub $ concat reqs)
