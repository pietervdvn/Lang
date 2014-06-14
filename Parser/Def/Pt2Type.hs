module Def.Pt2Type (pt2type) where

import StdDef
import Bnf.ParseTree
import Bnf
import Bnf.Converter hiding (convert)
import Control.Monad.Writer

import Def.Pt2Prelude
import Def.Def hiding (Tuple)
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
convert	ast	=  error $ "Still to do the convert! Here is the ast though:\n"++show ast


h		:: StdDef.Name -> ParseTree -> Maybe (Writer Errors AST)
h _		=  const Nothing

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
t nm cont	= error $ "Pt2Type: Token fallthrough for rule '"++nm++"' with content '"++cont++"'"


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
s nm ast	= error $ "Pt2Type: Sequence fallthrough for rule '"++nm++"' with content "++show ast


pt2ast	:: ParseTree -> Writer Errors AST
pt2ast	=  simpleConvert h t s

pt2type	:: ParseTree -> Writer Errors Type
pt2type	pt
	= do	ast <- pt2ast pt
		return $ convert ast

unpack	:: AST -> [AST]
unpack (CommaSepTypes asts)	= asts
unpack ast			= [ast]

asTuple	:: [AST] -> AST
asTuple [ast]	= ast
asTuple asts	= Tuple asts

hook		:: (ParseTree -> AST) -> ParseTree -> Maybe (Writer Errors AST)
hook f		=  Just . return . f 
