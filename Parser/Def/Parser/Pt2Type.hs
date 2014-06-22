module Def.Parser.Pt2Type (pt2type) where

import StdDef
import Bnf.ParseTree
import Bnf
import Control.Monad.Writer
import Def.Parser.Utils
import Def.Parser.Pt2Prelude
import Def.AST hiding (Tuple)
{--
This module parses Types! YAY
--}


data AST	= KnownType String
		| FreeType String
		| AppliedType AST [AST]
		| Tuple [AST]
		| CurryType [AST]
		| Unknown
		| ParO	| ParC
		| Comma	| CommaSepTypes [AST]
		| Arrow	| MultiType [AST]
		| MaybeT
		| Currow	-- Curry arrow
	deriving (Show)

convert		:: AST -> Type
convert (KnownType id)
		= Normal id
convert (FreeType id)
		= Free id
convert Unknown	= Infer
convert (AppliedType ast asts)
		= Applied (convert ast) (map convert asts)
convert (Tuple asts)
		= TupleType $ map convert asts
convert (CurryType asts)
		= Curry $ map convert asts
convert	ast	=  convErr "Pt2Type" ast

t		:: Name -> String -> AST
t "knownType" s	= KnownType s
t "freeType" s	= FreeType s
t "void"  _	= KnownType "Void"
t "infer" _	= Unknown
t _ "("		= ParO
t _ ")"		= ParC
t _ "["		= ParO
t _ "]"		= ParC
t _ "{"		= ParO
t _ "}"		= ParC
t _ ","		= Comma
t _ "-->"	= Arrow
t _ "->"	= Currow
t _ "?"		= MaybeT
t nm cont	= tokenErr "Pt2Type" nm cont

s		:: Name -> [AST] -> AST
s "simpleType" [ParO, typ, ParC]
		= typ
s "commaSepTypes" [Comma, typ]
		= typ
s "commaSepTypes" [typ]
		= typ
s "commaSepTypes" [typ, CommaSepTypes typs]
		= CommaSepTypes $ typ:typs
s "commaSepTypes" typs
		= CommaSepTypes typs
s "list" [ParO, cont, ParC]
		= AppliedType (KnownType "List") [asTuple $ unpack cont]
s "set" [ParO, cont, ParC]
		= AppliedType (KnownType "Set") [asTuple $ unpack cont]
s "dict" [ParO, keys, Arrow, vals, ParC]
		= AppliedType (KnownType "Dict") $ map (asTuple . unpack) [keys, vals]
s "tuple" [Comma, typ]
		= typ
s "tuple" [ParO, typ, CommaSepTypes types, ParC]
		= Tuple $ typ:types
s "tuple" typs	= CommaSepTypes typs
s "appliedType" [MultiType (ast:asts)]
		= AppliedType ast asts
s "appliedType" [ast]	= ast
s "appliedType" [typ, MultiType appliedTo]
		= AppliedType typ appliedTo
s "appliedType" types
		= MultiType types
s "curry" [Currow, typ]	
		= typ
s "curry" [typ, MultiType typs]
		= CurryType $ typ:typs
s "curry" typs	= MultiType typs
s "baseType" [ast, MaybeT]
		= AppliedType (KnownType "Maybe") [ast]
s _ [ast]  	= ast
s nm ast	= seqErr "Pt2Type" nm ast

unpack	:: AST -> [AST]
unpack (CommaSepTypes asts)	= asts
unpack ast			= [ast]

asTuple	:: [AST] -> AST
asTuple [ast]	= ast
asTuple asts	= Tuple asts


pt2type :: ParseTree -> Type
pt2type	= pt2a [] t s convert

