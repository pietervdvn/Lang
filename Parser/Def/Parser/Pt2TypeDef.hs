module Def.Parser.Pt2TypeDef (pt2syndef) where

import StdDef
import Bnf.ParseTree
import Bnf
import Def.Parser.Utils
import Def.Parser.Pt2Type
import Def.Parser.Pt2DataDef
import Def.AST

{--

This module converts the ParseTree into all other things defined in TypeDefs.bnf: type synonyms, subtypes, class declarations and instances.
--}

modName	= "Pt2TypeDef"
modNSD	= modName ++ "-SynDef"


pt2syndef	:: ParseTree -> SynDef
pt2syndef	=  pt2a h t s convert

convert		:: ASTSD -> SynDef
convert (Syn name frees typ)
		= SynDef name frees typ
convert ast	=  convErr modName ast


data ASTSD	= TypeT
		| Ident Name
		| FreeTypes [Name]
		| EqualT
		| Type Type
		| Syn Name [Name] Type
	deriving (Show)


h		:: [(Name, ParseTree -> ASTSD)]
h		=  [("freeTypes",FreeTypes . pt2freetypes),("type", Type . pt2type)]

t		:: Name -> String -> ASTSD
t "globalIdent" id
		= Ident id
t _ "type"	=  TypeT
t _ "="		=  EqualT
t nm cont	=  tokenErr modName nm cont


s		:: Name -> [ASTSD] -> ASTSD
s _ [TypeT,Ident id,FreeTypes frees,EqualT,Type t]
		= Syn id frees t
s _ [TypeT,Ident id,EqualT,Type t]
		= Syn id [] t
s _ [ast]	= ast
s nm asts	= seqErr modName nm asts




