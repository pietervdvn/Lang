module Languate.Parser.Pt2PrecedenceAnnot (pt2precAnnot) where

import StdDef
import Bnf.ParseTree
import Bnf
import Languate.Parser.Utils
import Languate.AST

{--
This module converts the ParseTree into a precedence annotation.
--}

modName	= "Pt2PrecedenceAnnot"

pt2precAnnot	:: ParseTree -> Annotation
pt2precAnnot	=  pt2a [] t s convert

convert		:: AST -> Annotation
convert	(PrecAnnotT annot)
		=  annot


data AST	= ParO | ParC
		| Op Name
		| LeftT	| RightT
		| PrefixT	| PostfixT
		| LTT	| EqT	| GTT
		| PrecT	| AtT	| CommaT
		| ColonT
		| PrecRel PrecRelation
		| PrecAnnotT Annotation
	deriving (Show)


t		:: Name -> String -> AST
t _ "@"		= AtT
t _ "precedence"
		= PrecT
t _ ":"		= ColonT
t _ ","		= CommaT
t _ "("		= ParO
t _ ")"		= ParC
t "parOp" op	= Op op
t "left" _	= LeftT
t "right" _	= RightT
t "prefix" _	= PrefixT
t "postfix" _	= PostfixT
t _ "<"		= LTT
t _ "="		= EqT
t _ ">"		= GTT
t _ "is"	= ColonT
t n cont	= tokenErr modName n cont

s		:: Name -> [AST] -> AST
s _ [ParO, Op op, ParC]
		= Op op
s _ [Op o1, rel, Op o2]
		= PrecRel $ relationToken2AST rel o1 o2
s _ [CommaT, ast]
		= ast
s _ (AtT:PrecT:ColonT:Op name:ColonT: mod: CommaT:rels)
		= PrecAnnotT $ PrecAnnot name (modToken2AST mod) $ map (\(PrecRel a) -> a) rels
s _ [ast]	= ast
s nm asts	= seqErr modName nm asts

relationToken2AST	:: AST -> Name -> Name -> PrecRelation
relationToken2AST EqT	=  PrecEQ
relationToken2AST LTT	=  PrecLT
relationToken2AST GTT	=  flip PrecLT

modToken2AST		:: AST -> PrecModifier
modToken2AST LeftT	= PrecLeft
modToken2AST RightT	= PrecRight
modToken2AST PrefixT	= PrecPrefix
modToken2AST PostfixT	= PrecPostfix
