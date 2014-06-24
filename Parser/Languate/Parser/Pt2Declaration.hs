module Languate.Parser.Pt2Declaration (pt2decl) where

import StdDef
import Bnf.ParseTree
import Bnf hiding (simpleConvert)
import Languate.Parser.Pt2Type
import Languate.AST
import Languate.Parser.Utils


{--

This module converts the ParseTree into a function declaration. This is a simple data structure, containing name and type.

functionName	:

is equivalent to

functionName	: _

--}

pt2decl	:: ParseTree -> (Name, Type)
pt2decl	= pt2a h t s convert

convert		:: AST -> (Name, Type)
convert (Decl nm t)
		= (nm, t)
convert ast	= convErr "pt2Declaration" ast

data AST	= Type Type
		| Ident Name
		| Colon	| ParO	| ParC
		| Decl Name Type
	deriving (Show)


h		:: [(Name, ParseTree -> AST)]
h 		= [("type", Type . pt2type)]

t		:: Name -> String -> AST
t "localIdent" id
		=  Ident id
t "op" id	=  Ident id
t _ ":"		=  Colon
t _ "("		=  ParO
t _ ")"		=  ParC
t nm cont	=  tokenErr "Pt2Declaration" nm cont


s		:: Name -> [AST] -> AST
s _ [ParO, Ident id, ParC]
		= Ident id
s _ [Ident nm, Colon, Type t]
		= Decl nm t
s _ [Ident nm, Colon]
		= Decl nm Infer
s _ [expr]	= expr
s nm exprs	= seqErr "Pt2Declaration" nm exprs




