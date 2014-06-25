module Languate.Parser.Pt2Statement (pt2stm) where

import StdDef
import Bnf.ParseTree
import Bnf
import Languate.Parser.Pt2Comment
import Languate.Parser.Pt2Function
import Languate.Parser.Pt2DataDef
import Languate.Parser.Pt2TypeDef
import Languate.Parser.Pt2ClassDef
import Languate.Parser.Utils
import Languate.AST

{--

This module converts the ParseTree into a function declaration, with laws etc.
Declarations may have multiple (explicit) types

--}

modName	= "Pt2Statement"

pt2stm	:: ParseTree -> [Statement]
pt2stm	=  pt2a h t s convert

convert		:: AST -> [Statement]
convert (Func f)
		= [FunctionStm f]
convert (ADTDf def)
		= [ADTDefStm def]
convert (SynDf def)
		= [SynDefStm def]
convert (Comms comms ast)
		= Comments comms:convert ast
convert (Comm comms)
		= [Comments comms]
convert (SubTypeDf def)
		= [SubDefStm def]
convert (ClassDf def)
		= [ClassDefStm def]


data AST	= Func Function
		| ADTDf ADTDef
		| SynDf SynDef
		| Comms [Comment] AST
		| Comm [Comment]
		| SubTypeDf SubDef
		| ClassDf ClassDef
	deriving (Show)


h		:: [(Name, ParseTree -> AST)]
h		=  [ ("nls",Comm . pt2nls),("function", unc Func pt2func)
		   , ("data",unc ADTDf pt2adtdef), ("synonym", SynDf . pt2syndef)
		   , ("subtype", SubTypeDf . pt2subdef), ("class", unc ClassDf pt2classDef)]

unc		:: (a -> AST) -> (ParseTree -> ([Comment], a)) -> ParseTree -> AST
unc constr f pt =  let (comms, a) = f pt in
			Comms comms $ constr a

t		:: Name -> String -> AST
t 		=  tokenErr modName

s		:: Name -> [AST] -> AST
s _ [Comm comms, ast]
		= Comms comms ast
s _ [ast]	= ast
s nm asts	= seqErr modName nm asts




