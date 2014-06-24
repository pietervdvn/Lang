module Languate.Parser.Pt2TypeDef (pt2syndef,pt2subdef) where

import StdDef
import Bnf.ParseTree
import Bnf
import Languate.Parser.Utils
import Languate.Parser.Pt2Type
import Languate.Parser.Pt2DataDef
import Languate.AST

{--

This module converts the ParseTree into all other things defined in TypeDefs.bnf: type synonyms, subtypes, class declarations and instances.
--}

-- ## GEN (typeExpr)


modName	= "Pt2TypeDef"

-- generic defs. Parses stuff of the format 'typeDef ::= globalIdent freeTypes "=" type'
pt2gendef	:: ParseTree -> (Name, [Name], Type)
pt2gendef	=  pt2a h t s convert

convert		:: AST -> (Name, [Name], Type)
convert (Stuff name frees typ)
		=  (name, frees, typ)
convert ast	=  convErr modName ast

data AST	= Ident Name
		| FreeTypes [Name]
		| EqualT
		| Type Type
		| Stuff Name [Name] Type
	deriving (Show)


h		:: [(Name, ParseTree -> AST)]
h		=  [("freeTypes",FreeTypes . pt2freetypes),("type", Type . pt2type)]

gh constr	= [("typeDef", constr . pt2gendef)]

t		:: Name -> String -> AST
t "globalIdent" id
		=  Ident id
t _ "="		=  EqualT
t nm cont	=  tokenErr modName nm cont


s		:: Name -> [AST] -> AST
s _ [Ident id,FreeTypes frees,EqualT,Type t]
		= Stuff id frees t
s _ [Ident id,EqualT,Type t]
		= Stuff id [] t
s _ [ast]	= ast
s nm asts	= seqErr modName
 nm asts

-- ## SYN DEF

pt2syndef	:: ParseTree -> SynDef
pt2syndef	=  pt2a (gh Syn) tsyn ssyn convSyn

data ASTSyn	= TypeT
		| Syn (Name,[Name],Type)

tsyn		:: Name -> String -> ASTSyn
tsyn _ "type"	=  TypeT

ssyn _ [TypeT, syn]
		= syn

convSyn (Syn (nm, frees,t))
		= SynDef nm frees t


-- ## SUB DEF

pt2subdef	:: ParseTree -> SubDef
pt2subdef	=  pt2a (gh Sub) tsub ssub convsub

data ASTsub	= SubTypeT
		| Sub (Name,[Name],Type)

tsub		:: Name -> String -> ASTsub
tsub _ "subtype"	=  SubTypeT

ssub _ [SubTypeT, sub]
		= sub

convsub (Sub (nm, frees,t))
		= SubDef nm frees t


