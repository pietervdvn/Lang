module Languate.Parser.Pt2Type (pt2type) where

import StdDef
import Bnf.ParseTree
import Bnf
import Languate.Parser.Utils
import Languate.AST hiding (Tuple)
{--
This module parses Types! YAY
--}


data AST	= KnownType [Name] String
		| Ident Name
		| FreeType String [AST]	-- name + constraints
		| AppliedType AST [AST]
		| Tuple [AST]
		| CurryType [AST]
		| Unknown
		| ParO	| ParC
		| Comma	| CommaSepTypes [AST]
		| ConjT	| Conj [AST]
		| Arrow	| MultiType [AST]
		| MaybeT	| CollectionT	| MoreT
		| InT	-- ''in'' - token, aka reqSep
		| Constraint [AST]
		| Currow	-- Curry arrow
		| DotT
	deriving (Show, Eq)

convert		:: AST -> (Type, [TypeRequirement])
convert (KnownType nms id)
		= noReq $ Normal nms id
convert (FreeType id constraints)
		= let (isTypes, reqs)	= unzip $ map convert constraints in
			(Free id, concat reqs ++ [(id, tp) | tp <- isTypes])
convert Unknown	= noReq DontCareType
convert (AppliedType ast asts)
		= packReqs (\tps -> Applied (head tps) (tail tps)) (ast:asts)
convert (Tuple asts)
		= packReqs TupleType asts
convert (CurryType asts)
		=  packReqs Curry asts
convert (Conj asts)
		= packReqs TypeConj asts
convert (MultiType [t])
		= convert t
convert	ast	=  convErr "Pt2Type" ast

noReq		:: Type -> (Type,[a])
noReq t		=  (t, [])

packReqs	:: ([Type] -> Type) -> [AST] -> (Type,[TypeRequirement])
packReqs constr asts
		=  let (tps, reqs)	= unzip $ map convert asts in
			(constr tps, concat reqs)

t		:: Name -> String -> AST
t "globalIdent" s
		= Ident s
t "typeIdent" s	= Ident s
t "freeType" s	= FreeType s []
t "unit"  _	= KnownType [] "Unit"
t "dontCare" _	= Unknown
t "subTypeT" _	= InT
t "typeConjTC" _
		= Comma
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
t _ "*"		= CollectionT
t _ "+"		= MoreT
t _ "."		= DotT
t _ "&"		= ConjT
t nm cont	= tokenErr "Pt2Type" nm cont

s		:: Name -> [AST] -> AST
s "knownType" [Ident s, DotT]
		= Ident s
s "knownType" idents
		= let names = concatMap unpackIdents idents in
			KnownType (init names) (last names)
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
s "typeConjT" [Comma, typ]
		= typ
s "typeConjT" [InT, typ]
		= typ
s "typeConjT" [typ]
		= typ
s "typeConjT" [typ, CommaSepTypes typs]
		= CommaSepTypes $ typ:typs
s "typeConjT" typs
		= CommaSepTypes typs

s "list" [ParO, cont, ParC]
		= AppliedType (KnownType [] "List") [asTuple $ unpack cont]
s "set" [ParO, cont, ParC]
		= AppliedType (KnownType [] "Set") [asTuple $ unpack cont]
s "dict" [ParO, keys, Arrow, vals, ParC]
		= AppliedType (KnownType [] "Dict") $ map (asTuple . unpack) [keys, vals]
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
-- _typeConj	::= appliedType (typeConjT typeConj)*
s "typeConj" [ConjT, typ]
		= typ
s "typeConj" [typ, Conj typs]
		= Conj $ typ:typs
s "typeConj" [Conj tps]
		= Conj tps
s "typeConj" typs
		= Conj typs

s "type" [Currow, typ]
		= typ
s "type" [typ, MultiType typs]
		= CurryType $ typ:typs
s "type" typs	= MultiType typs
s "baseType" [ast, MaybeT]
		= AppliedType (KnownType [] "Maybe") [ast]
s "baseType" [ast, CollectionT]
		= AppliedType (KnownType [] "Collection") [ast]
s "baseType" [ast, MoreT]
		= AppliedType (KnownType [] "More") [ast]
s "reqs" [Comma, ast]
		= ast
s "parFreeType" [FreeType name _, InT, ParO, CommaSepTypes types, ParC]
		= FreeType name types
s "parFreeType" [FreeType name _, InT, typ]
		= FreeType name [typ]
s _ [ast]  	= ast
s nm ast	= seqErr "Pt2Type" nm ast

unpack	:: AST -> [AST]
unpack (CommaSepTypes asts)	= asts
unpack ast			= [ast]

unpackIdents	:: AST -> [Name]
unpackIdents (Ident n)	= [n]
unpackIdents (KnownType nms n)
			= nms++[n]

concatConstraints	:: [AST] -> [AST]
concatConstraints (Constraint consts:tail)
			= consts ++ concatConstraints tail
concatConstraints (Comma:tail)
			= concatConstraints tail
concatConstraints (a:tail)
			= a:concatConstraints tail
concatConstraints []	= []

asTuple	:: [AST] -> AST
asTuple [ast]	= ast
asTuple asts	= Tuple asts


pt2type :: ParseTree -> (Type, [TypeRequirement])
pt2type	= pt2a [] t s convert
