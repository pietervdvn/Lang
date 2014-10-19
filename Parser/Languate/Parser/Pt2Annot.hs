module Languate.Parser.Pt2Annot (pt2annot)where

import StdDef
import Bnf.ParseTree
import Bnf
import Languate.Parser.Pt2PrecedenceAnnot
import Languate.Parser.Utils
import Languate.AST

{--

This module converts the ParseTree into an annotation.
--}

modName	= "Pt2Annot"


pt2annot	:: ParseTree -> Annotation
pt2annot	= pt2a [("precedenceAnnot", pt2precAnnot),("normalAnnotation", pt2normAnnot)] (tokenErr $ "top level:"++ modName) seq' id


seq'		:: Name -> [Annotation] -> Annotation
seq' _ [ast]	=  ast
seq' name asts	=  seqErr ("top level:" ++ modName) name asts

pt2normAnnot	:: ParseTree -> Annotation
pt2normAnnot	=  pt2a [] t s convert

convert		:: AST -> Annotation
convert	(Annot name cont)
		=  Annotation name cont


data AST	= AtT	| PrecT
		| ColonT
		| Ident Name
		| Cont String
		| Annot Name String
	deriving (Show)


t		:: Name -> String -> AST
t _ "@"		= AtT
t _ ":"		= ColonT
t "ident" name	= Ident name
t _ cont	= Cont cont

s		:: Name -> [AST] -> AST
s _ [AtT, Ident name, ColonT, Cont cont]
		= Annot name cont
s _ [ast]	= ast
s nm asts	= seqErr modName nm asts