module Languate.Parser.Pt2Statement (pt2stm) where

import StdDef
import Bnf.ParseTree
import Bnf
import Normalizable

import Languate.Parser.Pt2Function
import Languate.Parser.Pt2Law
import Languate.Parser.Pt2Comment
import Languate.Parser.Pt2DataDef
import Languate.Parser.Pt2SubTypeDef
import Languate.Parser.Pt2ClassDef
import Languate.Parser.Pt2Annot
import Languate.Parser.Pt2Precedence
import Languate.Parser.Utils
import Languate.AST

{--

This module converts the ParseTree into a function declaration, with laws etc.
Declarations may have multiple (explicit) types

--}

modName	= "Pt2Statement"

pt2stm	:: ParseTree -> [Statement]
pt2stm	=  nss . fmap normalize . pt2a h t s convert

convert		:: AST -> [Statement]
convert (Func f)
		= [FunctionStm f]
convert (Lw l)	= [LawStm l]
convert (ADTDf def)
		= [ADTDefStm def]
convert (Comms comms ast)
		= Comments comms:convert ast
convert (Comm comms)
		= [Comments comms]
convert (Docs docstrs ast)
		= DocStringStm docstrs:  convert ast
convert (SubTpDf def)
		= [SubDefStm def]
convert (ClassDf def)
		= [ClassDefStm def]
convert (Annot annot)
		= [AnnotationStm annot]
convert (PrecAnn prec)
		= [PrecedenceStm prec]
convert (InstanceAST inst)
		= [InstanceStm inst]


data AST	= Func Function
		| Lw Law
		| ADTDf ADTDef
		| Docs [DocString (Name, Name)] AST
		| Comms [Comment] AST
		| Comm [Comment]
		| SubTpDf SubDef
		| ClassDf ClassDef
		| Annot Annotation
		| PrecAnn PrecedenceAnnot
		| InstanceAST Instance
	deriving (Show)


h		:: [(Name, ParseTree -> AST)]
h		=  [ ("nls",		Comm 	. pt2nls)
		   , ("function",   	Func 	. pt2func)
		   , ("law",		Lw	. pt2law)
		   , ("data",	    unc ADTDf 	  pt2adtdef)
		   , ("subtype", 	SubTpDf . pt2subdef)
		   , ("cat", 	    unc ClassDf   pt2classDef)
		   , ("annotation", 	Annot 	. pt2annot)
		   , ("precedence", 	PrecAnn . pt2precedence)
		   , ("instance", 	InstanceAST . pt2instance)]

unc		:: (a -> AST) -> (ParseTree -> ([DocString (Name, Name)], a)) -> ParseTree -> AST
unc constr f pt =  let (docs, a) = f pt in
			Docs docs $ constr a

t		:: Name -> String -> AST
t 		=  tokenErr modName

s		:: Name -> [AST] -> AST
s _ [Comm comms, ast]
		= Comms comms ast
s _ [ast]	= ast
s nm asts	= seqErr modName nm asts
