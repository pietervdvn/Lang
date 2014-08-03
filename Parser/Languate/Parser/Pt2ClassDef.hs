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

{--

This module converts the ParseTree into a class def and Instance.

See bnf for usage

--}

modName	= "Pt2ClassDef"

pt2classDef	:: ParseTree -> ([Comment], ClassDef)
pt2classDef	=  pt2a h t s convert . cleanAll ["nltab","nl"]

convert		:: AST -> ([Comment], ClassDef)
convert (Clss name inFuncName comms laws declarations)
		=  (init' comms, ClassDef name inFuncName (last comms) laws declarations)
convert ast	=  convErr modName ast


data AST	= Clss Name Name [Comment] [Law] [(Name,Type,Maybe Comment)]
		| Body [Law] [(Name,Type,Maybe Comment)]
		| Ident Name	| LIdent Name
		| Lw Law	| Decl (Name, Type, Visible)
		| FreeT [Name]	| Comms [Comment]
		| ClassT
		| ColonT
	deriving (Show)


h		:: [(Name, ParseTree -> AST)]
h		=  [("nlcomments", Comms . pt2nlcomments), ("comment", Comms . (:[]) . pt2comment), ("mlcomment", Comms . (:[]) . pt2comment)
		   , ("law", Lw . pt2law ),("declaration", Decl . pt2decl),("freeTypes",FreeT . pt2freetypes)]

t		:: Name -> String -> AST
t "globalIdent" n
		=  Ident n
t "localIdent" id
		=  LIdent id
t _ ":"		=  ColonT
t _ "class"	=  ClassT
t nm cont	=  tokenErr modName nm cont


s		:: Name -> [AST] -> AST
s _ [ast]	= ast
s "classBody" asts
		= uncurry Body $ triage asts Nothing
s _ [Comms comms, ClassT, Ident name, LIdent lname, ColonT, Body laws decls]
		= Clss name lname comms laws decls
s nm asts	= seqErr modName nm asts


triage		:: [AST] -> Maybe Comment -> ([Law], [(Name,Type, Maybe Comment)])
triage [] _
		= ([], [])
triage (Lw law:tail) comm
		= first (law:) $ triage tail comm
triage (Decl (n,t,v):tail) comm
	| v == Private	= error "No private functions allowed in classdefs!"
	| otherwise	= second ((n,t,comm):) $ triage tail comm
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
		| IIdent Name	| Type Type
		| InstanceT
	deriving (Show)


hi		:: [(Name, ParseTree -> ASTi)]
hi		=  [("baseType", Type . pt2type)]

ti		:: Name -> String -> ASTi
ti "globalIdent" n
		=  IIdent n
ti _ "instance"	=  InstanceT
ti nm cont	=  tokenErr (modName++"i") nm cont


si		:: Name -> [ASTi] -> ASTi
si _ [InstanceT, IIdent name, Type t]
		= Inst name t
si _ [ast]	= ast
si nm asts	= seqErr modName nm asts
