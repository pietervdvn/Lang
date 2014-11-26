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

{--

This module converts the ParseTree into a class def and Instance.

See bnf for usage

--}

modName	= "Pt2ClassDef"

pt2classDef	:: ParseTree -> ([Comment], ClassDef)
pt2classDef	=  pt2a h t s convert . cleanAll ["nltab","nl"]

convert		:: AST -> ([Comment], ClassDef)
convert (Clss name frees subs reqs comms laws declarations)
		=  (init' comms, ClassDef name frees reqs subs (last comms) laws $ injectTypeReq name declarations)
convert ast	=  convErr modName ast

injectTypeReq	:: Name -> [(Name, Type, Maybe Comment, [TypeRequirement])] -> [(Name, Type, Maybe Comment, [TypeRequirement])]
injectTypeReq (n:nm) decls
		= let	typeReq	= nub [(toLower n : nm, Normal [] $ n:nm), (map toLower (n:nm), Normal [] $ n:nm)]	in
			map (\(n,t,mc, treqs) -> (n,t,mc, typeReq ++ treqs)) decls


data AST	= Clss Name [Name] [Type] [TypeRequirement] [Comment] [Law] [(Name, Type, Maybe Comment, [TypeRequirement])] -- ""class Dict (k:Ord) v in Collection v:"" => Clss "Dict" ["k","v"] ["Collection"]
		| Body [Law] [(Name,Type,Maybe Comment, [TypeRequirement])]
		| Ident Name
		| Lw Law
		| SubClassOf [Type] [TypeRequirement]
		| Decl (Name, Type, Visible, [TypeRequirement])
		| FreeT [Name] [TypeRequirement]
		| Type Type [TypeRequirement]
		| Comms [Comment]
		| ClassT	| SubClassT
		| ColonT	| CommaT
	deriving (Show)


h		:: [(Name, ParseTree -> AST)]
h		= 	[ ("nlcomments"	, Comms . pt2nlcomments)
			, ("comment"	, Comms . (:[]) . pt2comment)
			, ("mlcomment"	, Comms . (:[]) . pt2comment)
		   	, ("law"	, Lw    . pt2law )
			, ("declaration", Decl  . pt2decl)
			, ("type"	, uncurry Type 	. pt2type)
			, ("freeTypes"	, uncurry FreeT . pt2freetypes)]

t		:: Name -> String -> AST
t "globalIdent" n
		=  Ident n
t _ ":"		=  ColonT
t _ "class"	=  ClassT
t "subclass" "in"
		= SubClassT
t "subclass" "is"
		= SubClassT
t nm cont	=  tokenErr modName nm cont


s		:: Name -> [AST] -> AST
s "classBody" asts
		= uncurry Body $ triage asts Nothing
s "subclass" (SubClassT:Type t reqs:tail)
		= let SubClassOf ts reqs'	= s "subclass" tail in
			SubClassOf (t:ts) (reqs++reqs')
s "subclass" [CommaT, Type t reqs]
		= SubClassOf [t] reqs
s "subclass" []	= SubClassOf [] []
s _ [ast]	= ast
s _ [Comms comms, ClassT, Ident name, FreeT freeNames reqs, SubClassOf subs reqs', ColonT, Body laws decls]
		= Clss name freeNames subs (reqs++reqs') comms laws decls
s _ [Comms comms, ClassT, Ident name, FreeT names reqs, ColonT, Body laws decls]
		= Clss name names [] reqs comms laws decls
s _ [Comms comms, ClassT, Ident name, SubClassOf subs reqs, ColonT, Body laws decls]
		= Clss name [] subs reqs comms laws decls
s _ [Comms comms, ClassT, Ident name, ColonT, Body laws decls]
		= Clss name [] [] [] comms laws decls
s _ (ClassT:Ident name:_)
		= error $ "No docstring comment with class definition of "++show name
s nm asts	= seqErr modName nm asts


triage		:: [AST] -> Maybe Comment -> ([Law], [(Name,Type, Maybe Comment, [TypeRequirement])])
triage [] _
		= ([], [])
triage (Lw law:tail) comm
		= first (law:) $ triage tail comm
triage (Decl (n,t,v,reqs):tail) comm
	| v == Private	= error "No private functions allowed in classdefs!"
	| otherwise	= second ((n,t,comm,reqs):) $ triage tail comm
triage (Body laws decls:tail) comm
		= first (laws++) $ second (decls++) $ triage tail comm
triage (Comms []:tail) comm
		= triage tail comm
triage (Comms (a:_):tail) _
		= triage tail (Just a)


-- INSTANCE DEF --
------------------

pt2instance	:: ParseTree -> Instance
pt2instance	=  pt2a hi ti si convi



convi		:: ASTi -> Instance
convi (Inst name t)
		=  Instance name t
convi ast	=  convErr modName ast


data ASTi	= Inst Name Type
		| IIdent Name	| Typei Type
		| InstanceT
	deriving (Show)


hi		:: [(Name, ParseTree -> ASTi)]
hi		=  [("baseType", Typei . check . pt2type)]

check		:: (Type, [TypeRequirement]) -> Type
check (t,[])	=  t
check (t,reqs)	= error $ "Type requirements are not allowed in instance declarations: "++show t++" "++show reqs

ti		:: Name -> String -> ASTi
ti "globalIdent" n
		=  IIdent n
ti _ "instance"	=  InstanceT
ti nm cont	=  tokenErr (modName++"i") nm cont


si		:: Name -> [ASTi] -> ASTi
si _ [InstanceT, IIdent name, Typei t]
		= Inst name t
si _ [ast]	= ast
si nm asts	= seqErr modName nm asts
