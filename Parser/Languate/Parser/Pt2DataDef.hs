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

import Control.Arrow

{--

This module converts the ParseTree into a data def. The heavy lifting --parsing the constructors-- is done by pt2datadefprod
--}

modName	= "Pt2DataDef"

pt2adtdef	:: ParseTree -> ([DocString (Name, Name)],ADTDef)
pt2adtdef	=  pt2a h t s convert

convert		:: AST -> ([DocString (Name,Name)], ADTDef)
convert (Data vis name frees sums reqs docs adoptions)
		=  let  edit	= if vis == Private then map (setVisibility vis) else id in
			(docs ||>> (const name &&& id), ADTDef name frees reqs (edit sums) adoptions)
convert ast	=  convErr modName ast


data AST	= Ident Name
		| FreeTypes [Name] [TypeRequirement]
		| Prod [ADTSum] [TypeRequirement] [DocString Name]
		| Data {mode::Visible, nm::Name, frees_::[Name], constructors::[ADTSum], reqs::[TypeRequirement], docs::[DocString Name], adoptions::[Type]}
		| TypeT	| PrivT	| EqualT
		| TypeV (Type, [TypeRequirement])
		| Adopted [(Type, [TypeRequirement])]
		| PlusT	| BraceT
	deriving (Show)


h		:: [(Name, ParseTree -> AST)]
h		=  	[ ("prod", uncurry3 Prod . pt2prod)
			, ("freeTypes", uncurry FreeTypes . pt2freetypes)
			, ("type",TypeV . pt2type)]

t		:: Name -> String -> AST
t "globalIdent" id
		=  Ident id
t "typeIdent" id
		=  Ident id
t "localIdent" id
		=  FreeTypes [id] []
t _ "type"	=  TypeT
t _ "_"		=  PrivT
t _ "="		=  EqualT
t _ "+"		=  PlusT
t nm cont	=  tokenErr modName nm cont


s		:: Name -> [AST] -> AST
s r (TypeT:PrivT:tail)
		= let decl = s r (TypeT:tail) in
			decl {mode = Private}
s r (TypeT:Ident name:FreeTypes frees' reqs':tail)
		=  let decl = s r (TypeT:Ident name:tail) in
			decl {frees_ = frees', reqs = reqs decl ++ reqs'}
s r (TypeT:Ident name:EqualT:Prod sums rqs dcs:rest)
		= let	decl 	= s r (TypeT:Ident name:EqualT:rest) in
			decl{constructors = constructors decl ++ sums,
				reqs = reqs decl ++ rqs,
				docs = docs decl ++ dcs}
s r (TypeT:Ident name:EqualT:rest)
		= let	decl	= case s r rest of
					(Adopted adops)	-> _adops2data adops
					decl	-> decl
			in
			decl {nm = name}
s r (Prod sums rqs dcs:rest)
		= let 	Adopted adops	= s r rest
			decl = _adops2data adops in
			decl {constructors = constructors decl ++ sums,
				reqs	= reqs decl ++ rqs,
				docs	= docs decl ++ dcs}
s r (decl@(Data{}):rest)
		= let	Adopted adopReq	= s r rest
			(adops, rqs)	= unzip adopReq |> concat in
			decl{adoptions = adoptions decl ++ adops,
				reqs = reqs decl ++ rqs}

s r (Adopted adops:Adopted adops':rest)
		= s r (Adopted (adops ++ adops'):rest)
s _ []		= Adopted []
s r (PlusT:rest)
		= s r rest
s r (TypeV v:rest)
		= let Adopted tail	= s r rest in
			Adopted (v:tail)
s _ [ast]	= ast
s nm asts	= seqErr modName nm asts

_adops2data	:: [(Type, [TypeRequirement])] -> AST
_adops2data adopReqs
	= let	(adops, reqs)	= adopReqs & unzip |> concat
		nameErr	= "Parser error in "++modName++"(No name given)" in
		Data Public (nameErr) [] [] reqs [] adops


pt2freetypes	:: ParseTree -> ([Name],[TypeRequirement])
pt2freetypes	=  pt2a [("constrFreeType", uncurry Id . pt2type)] (tokenErr "No tokenize should be needed on freeTypes") sf conv

sf		:: Name -> [ASTf] -> ASTf
sf _		=  uncurry Ids . unzip . map (\(Id (Free id) reqs) -> (id,reqs))

data ASTf	= Id Type [TypeRequirement]
		| Ids [Name] [[TypeRequirement]]

conv		:: ASTf -> ([Name], [TypeRequirement])
conv (Ids nms reqs)	=  (nms,nub $ concat reqs)
