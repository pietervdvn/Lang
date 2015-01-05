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

pt2adtdef	:: ParseTree -> ([DocString (Name, Name)],ADTDef)
pt2adtdef	=  pt2a h t s convert

convert		:: AST -> ([DocString (Name,Name)], ADTDef)
convert (Data vis name frees sums reqs docs)
		=  let  edit	= if vis == Private then map (setVisibility vis) else id in

			(map (fmap $ \cons -> (name,cons)) docs, ADTDef name frees reqs $ edit sums)
convert ast	=  convErr modName ast


data AST	= Ident Name
		| FreeTypes [Name] [TypeRequirement]
		| Prod [ADTSum] [TypeRequirement] [DocString Name]
		| Data Visible Name [Name] [ADTSum] [TypeRequirement] [DocString Name]
		| DataT	| PrivT	| EqualT
	deriving (Show)


h		:: [(Name, ParseTree -> AST)]
h		=  	[ ("prod", uncurry3 Prod . pt2prod)
			, ("freeTypes", uncurry FreeTypes . pt2freetypes)]

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

s _ (DataT:PrivT:tail)
		= let Data _ name freetypes sums reqs docs = s "data" (DataT:tail) in
			Data Private name freetypes sums reqs docs
s _ (DataT:Ident name:FreeTypes frees reqs:tail)
		=  let Data vis name' _ sums reqs' docs = s "data" (DataT:Ident name:tail) in
			Data vis name' frees sums (reqs ++ reqs') docs
s _ [DataT, Ident name,EqualT, Prod sums reqs docs]
		= Data Public name [] sums reqs docs
s _ [ast]	= ast
s nm asts	= seqErr modName nm asts

pt2freetypes	:: ParseTree -> ([Name],[TypeRequirement])
pt2freetypes	=  pt2a [("constrFreeType", uncurry Id . pt2type)] (tokenErr "No tokenize should be needed on freeTypes") sf conv

sf		:: Name -> [ASTf] -> ASTf
sf _		=  uncurry Ids . unzip . map (\(Id (Free id) reqs) -> (id,reqs))

data ASTf	= Id Type [TypeRequirement]
		| Ids [Name] [[TypeRequirement]]

conv		:: ASTf -> ([Name], [TypeRequirement])
conv (Ids nms reqs)	=  (nms,nub $ concat reqs)
