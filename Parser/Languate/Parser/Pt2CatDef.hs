module Languate.Parser.Pt2CatDef (pt2catDef, pt2instance) where

import StdDef
import Bnf.ParseTree
import Bnf
import Languate.Parser.Utils
import Languate.Parser.Pt2Law
import Languate.Parser.Pt2Declaration
import Languate.Parser.Pt2DataDef
import Languate.Parser.Pt2Comment
import Languate.Parser.Pt2Type

import Languate.AST
import Control.Arrow

import Data.Char
import Data.List (nub)

{--

This module converts the ParseTree into a class def and Instance.

See bnf for usage

--}

modName	= "Pt2CatDef"

pt2catDef	:: ParseTree -> ([DocString (Name, Name)], CatDef)
pt2catDef	=  pt2a h t s convert . cleanAll ["nltab","nl"]

convert		:: AST -> ([DocString (Name, Name)], CatDef)
convert (Clss name frees supers reqs asts)
		=  let 	(laws, declarations, docs)	= triage name asts
			classDef	= CatDef name frees reqs (supers >>= topLevelConj) laws $ injectTypeReq name declarations in
			(docs, classDef)
convert ast	=  convErr modName ast


-- injects the syntactic sugar that 'cat Abc\n\t abc -> abc' means '(abc:Abc)'
injectTypeReq	:: Name -> [(Name, Type, [TypeRequirement])] -> [(Name, Type, [TypeRequirement])]
injectTypeReq (n:nm) decls
		= let	typeReq	= nub   [ (toLower n : nm, Normal [] $ n:nm)
					, (map toLower (n:nm), Normal [] $ n:nm)]	in
			map (\(n,t, treqs) -> (n,t, typeReq ++ treqs)) decls


data AST	= Clss Name [Name] [Type] [TypeRequirement] [AST]
		| Body [AST]
		| Ident Name
		| Lw Law
		| Comm Comment
		| SubCatOf [Type] [TypeRequirement]
		| Decl (Name, Type, Visible, [TypeRequirement])
		| FreeT [Name] [TypeRequirement]
		| Type Type [TypeRequirement]
		| CatT	| SubCatT
		| CommaT
		| NlT
	deriving (Show)


h		:: [(Name, ParseTree -> AST)]
h		= 	[ ("law"	, Lw    . pt2law )
			, ("declaration", Decl  . pt2decl)
			, ("type"	, uncurry Type 	. pt2type)
			, ("freeTypes"	, uncurry FreeT . pt2freetypes)
			, ("comment"	, Comm . pt2comment)
			, ("mlcomment"	, Comm . pt2comment)]

t		:: Name -> String -> AST
t "typeIdent" n
		=  Ident n
t "globalIdent" n
		=  Ident n
t _ ('\n':_)	=  NlT
t _ "cat"	=  CatT
t _ "category" = CatT
t "subTypeWords" _	= SubCatT
t nm cont	=  tokenErr modName nm cont


s		:: Name -> [AST] -> AST
s "categoryStm" [NlT, ast]
		= ast
s "categoryBody" asts
		= Body asts
s "subclass" (SubCatT:Type t reqs:tail)
		= let SubCatOf ts reqs'	= s "subclass" tail in
			SubCatOf (t:ts) (reqs++reqs')
s "subclass" [CommaT, Type t reqs]
		= SubCatOf [t] reqs
s "subclass" []	= SubCatOf [] []
s _ [ast]	= ast
s _ [CatT, Ident name, FreeT freeNames reqs, SubCatOf subs reqs', Body asts]
		= Clss name freeNames subs (reqs++reqs') asts
s _ [CatT, Ident name, FreeT names reqs,  Body asts]
		= Clss name names [] reqs asts
s _ [CatT, Ident name, SubCatOf subs reqs, Body asts]
		= Clss name [] subs reqs asts
s _ [CatT, Ident name, Body asts]
		= Clss name [] [] [] asts
s nm asts	= seqErr modName nm asts


triage		:: Name -> [AST] -> ([Law], [(Name, Type, [TypeRequirement])], [DocString (Name, Name)])
triage _ []
		= ([], [], [])
triage catName 	(Lw law:tail)
		= first3 (law:) $ triage catName tail
triage catName (Decl (n,t,v,reqs):tail)
		= second3 ((n, t,reqs):) $ triage catName tail
triage catName (Comm c:tail)
		= let 	(lws, sign, oldDocs)	= triage catName tail
			newDocs	= buildDocsFor catName c (map fst3 sign) oldDocs in
			(lws, sign, oldDocs ++ newDocs )
triage catName [Body asts]
		= triage catName asts



buildDocsFor 	:: Name -> Comment -> [Name] -> [DocString (Name, Name)] -> [DocString (Name, Name)]
buildDocsFor gizmo doc need alreadyHave
		= let	alreadyHaveNms	= map (snd . about) alreadyHave in
			map (\n -> DocString doc (gizmo, n)) $ filter (`notElem` alreadyHaveNms) need



-- INSTANCE DEF --
------------------

pt2instance	:: ParseTree -> Instance
pt2instance	=  pt2a hi ti si convi



convi		:: ASTi -> Instance
convi (Inst id frees t reqs)
		=  Instance id frees t reqs
convi ast	=  convErr modName ast


data ASTi	= Inst ([Name],Name) [Name] Type [TypeRequirement]
		| TypeT Type [TypeRequirement]
		| TypeId ([Name],Name)
		| FreeIT Name [TypeRequirement]
		| Frees [Name] [TypeRequirement]
		| InstanceT	| SubT
	deriving (Show)


hi		:: [(Name, ParseTree -> ASTi)]
	-- basetype: the type which gets instantiated; knownType: the type which is the supertype of basetype
hi		=  [("type", uncurry TypeT . pt2type)
			, ("knownType", TypeId . extractId . fst . pt2type)
			, ("constrFreeType", uncurry FreeIT . extractFree . pt2type)]

ti		:: Name -> String -> ASTi
ti _ "instance"	=  InstanceT
ti "subTypeT" _	=  SubT
ti "subTypeWords" _
		= SubT

ti nm cont	=  tokenErr (modName++"i") nm cont


si		:: Name -> [ASTi] -> ASTi
si _ [InstanceT, TypeId id, Frees frees treqs, SubT, TypeT super treqs']
		= Inst id frees super $ treqs++treqs'
si _ [InstanceT, TypeId id, SubT, TypeT super treqs']
		= Inst id [] super treqs'
si _ all@(FreeIT n reqs:_)
		= uncurry Frees $ concatFrees all
si _ [ast]	= ast
si nm asts	= seqErr modName nm asts



concatFrees	:: [ASTi] -> ([Name], [TypeRequirement])
concatFrees (FreeIT nm reqs:tail)
		= let	(nm', reqs') = concatFrees tail in
			(nm:nm', reqs ++ reqs')
concatFrees _	= ([],[])


extractId	:: Type -> ([Name], Name)
extractId (Normal path nm)
		= (path, nm)

extractFree (Free a, treqs)
		= (a,treqs)
