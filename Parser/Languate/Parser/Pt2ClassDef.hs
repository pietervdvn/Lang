module Languate.Parser.Pt2ClassDef (pt2classDef, pt2instance) where

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

modName	= "Pt2ClassDef"

pt2classDef	:: ParseTree -> ClassDef
pt2classDef	=  pt2a h t s convert . cleanAll ["nltab","nl"]

convert		:: AST -> ClassDef
convert (Clss name frees subs reqs laws declarations)
		=  ClassDef name frees reqs subs laws $ injectTypeReq name declarations
convert ast	=  convErr modName ast


-- injects the syntactic sugar that 'cat Abc\n\t abc -> abc' means '(abc:Abc)'
injectTypeReq	:: Name -> [(Name, Type, [TypeRequirement])] -> [(Name, Type, [TypeRequirement])]
injectTypeReq (n:nm) decls
		= let	typeReq	= nub [(toLower n : nm, Normal [] $ n:nm), (map toLower (n:nm), Normal [] $ n:nm)]	in
			map (\(n,t, treqs) -> (n,t, typeReq ++ treqs)) decls


data AST	= Clss Name [Name] [Type] [TypeRequirement] [Law] [(Name, Type, [TypeRequirement])] -- ""class Dict (k:Ord) v in Collection v:"" => Clss "Dict" ["k","v"] ["Collection"]
		| Body [Law] [(Name,Type, [TypeRequirement])]
		| Ident Name
		| Lw Law
		| SubClassOf [Type] [TypeRequirement]
		| Decl (Name, Type, Visible, [TypeRequirement])
		| FreeT [Name] [TypeRequirement]
		| Type Type [TypeRequirement]
		| ClassT	| SubClassT
		| ColonT	| CommaT
	deriving (Show)


h		:: [(Name, ParseTree -> AST)]
h		= 	[ ("law"	, Lw    . pt2law )
			, ("declaration", Decl  . pt2decl)
			, ("type"	, uncurry Type 	. pt2type)
			, ("freeTypes"	, uncurry FreeT . pt2freetypes)]

t		:: Name -> String -> AST
t "typeIdent" n
		=  Ident n
t "globalIdent" n
		=  Ident n
t _ ":"		=  ColonT
t _ "cat"	=  ClassT
t _ "category" = ClassT
t "subclass" "in"
		= SubClassT
t "subclass" "is"
		= SubClassT
t nm cont	=  tokenErr modName nm cont


s		:: Name -> [AST] -> AST
s "classBody" asts
		= uncurry Body $ triage asts
s "subclass" (SubClassT:Type t reqs:tail)
		= let SubClassOf ts reqs'	= s "subclass" tail in
			SubClassOf (t:ts) (reqs++reqs')
s "subclass" [CommaT, Type t reqs]
		= SubClassOf [t] reqs
s "subclass" []	= SubClassOf [] []
s _ [ast]	= ast
s _ [ClassT, Ident name, FreeT freeNames reqs, SubClassOf subs reqs', Body laws decls]
		= Clss name freeNames subs (reqs++reqs') laws decls
s _ [ClassT, Ident name, FreeT names reqs,  Body laws decls]
		= Clss name names [] reqs laws decls
s _ [ClassT, Ident name, SubClassOf subs reqs, Body laws decls]
		= Clss name [] subs reqs laws decls
s _ [ClassT, Ident name, Body laws decls]
		= Clss name [] [] [] laws decls
s nm asts	= seqErr modName nm asts


triage		:: [AST] -> ([Law], [(Name,Type, [TypeRequirement])])
triage []
		= ([], [])
triage (Lw law:tail)
		= first (law:) $ triage tail
triage (Decl (n,t,v,reqs):tail)
		= second ((n,t,reqs):) $ triage tail
triage (Body laws decls:tail)
		= first (laws++) $ second (decls++) $ triage tail

-- INSTANCE DEF --
------------------

pt2instance	:: ParseTree -> Instance
pt2instance	=  pt2a hi ti si convi



convi		:: ASTi -> Instance
convi (Inst name t reqs)
		=  Instance name t reqs
convi ast	=  convErr modName ast


data ASTi	= Inst Type Type [TypeRequirement]
		| IIdent Type	| Typei Type [TypeRequirement]
		| InstanceT
	deriving (Show)


hi		:: [(Name, ParseTree -> ASTi)]
	-- basetype: the type which gets instantiated; knownType: the type which is the supertype of basetype
hi		=  [("baseType", uncurry Typei . pt2type), ("knownType", IIdent . fst . pt2type)]

check		:: (Type, [TypeRequirement]) -> Type
check (t,[])	=  t
check (t,reqs)	= error $ "Type requirements are not allowed in instance declarations: "++show t++" "++show reqs

ti		:: Name -> String -> ASTi
ti _ "instance"	=  InstanceT
ti nm cont	=  tokenErr (modName++"i") nm cont


si		:: Name -> [ASTi] -> ASTi
si _ [InstanceT, IIdent id, Typei t tr]
		= Inst id t tr
si _ [ast]	= ast
si nm asts	= seqErr modName nm asts
