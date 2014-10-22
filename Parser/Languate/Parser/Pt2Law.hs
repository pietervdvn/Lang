module Languate.Parser.Pt2Law (pt2law) where

import StdDef
import Bnf.ParseTree
import Bnf
import Languate.Parser.Utils
import Languate.Parser.Pt2Type
import Languate.Parser.Pt2Expr
import Languate.AST

import Data.Maybe
{--

This module converts the ParseTree into a law/example

--}

convert		:: AST -> Law
convert (DiffLaw nm declarations e1 e2)
		= Law nm declarations e1 $ toTrue e2
convert(SimpleLaw nm e1 e2)
		= Example nm e1 $ toTrue e2
convert ast	= convErr "Pt2Law" ast

toTrue		:: Maybe Expression -> Expression
toTrue		=  fromMaybe (Call "True")


data AST	= Tilde	| Bird	| Colon	| Equals
		| ForAll	| Comma
		| LawName Name
		| Ident Name	| Idents [Name]
		| Type Type
		| TypeDecl [(Name, Maybe Type)]
		| Expr Expression
		| RightExpr Expression
		-- simplelaw: has no declarations, thus easy for the interpreter
		| DiffLaw (Maybe Name) [(Name, Maybe Type)] Expression (Maybe Expression)
		| SimpleLaw (Maybe Name) Expression (Maybe Expression)
	deriving (Show)


h		:: [(Name, ParseTree -> AST)]
h		=  [("expr", Expr . pt2expr),("type", Type . pt2type)]

t		:: Name -> String -> AST
t _ "~"		=  Tilde
t _ ">"		=  Bird
t _ ":"		=  Colon
t _ "="		=  Equals
t _ "=>"	=  ForAll
t _ ","		=  Comma
t "localIdent" nm
		=  Ident nm
t "lawname" nm	=  LawName nm
t nm cont	=  tokenErr "Pt2Law" nm cont


s		:: Name -> [AST] -> AST
s _ [Colon, ast]= ast
s _ [Comma, ast]= ast
s "lawDeclaration" asts
		=  makeLawDecl asts []
s "lawDeclarations" asts
		= mergeTypeDecl asts []
s "law" [typeDecls, ForAll]
		= typeDecls
s "law" (LawName name : TypeDecl decls : Expr e1 :maybeRightExpr)
		= DiffLaw (Just name) decls e1 $ rightExpr maybeRightExpr
s "law" (LawName name : Expr e1 : maybeRightExpr)
		= SimpleLaw (Just name) e1 $ rightExpr maybeRightExpr
s "law" (Bird : Expr e1 : maybeRightExpr)
		= SimpleLaw Nothing e1 $ rightExpr maybeRightExpr
s "law" (Bird : TypeDecl decls : Expr e1 : maybeRightExpr)
		= DiffLaw Nothing decls e1 $ rightExpr maybeRightExpr
s "law" [Equals, Expr e]
		= RightExpr e
s "law" [Tilde, LawName nm, Colon]
		= LawName nm
s _ [ast]  	= ast
s nm asts	= seqErr "Pt2Law" nm asts

rightExpr	:: [AST] -> Maybe Expression
rightExpr []	=  Nothing
rightExpr [RightExpr e]
		= Just e

makeLawDecl	:: [AST] -> [Name] -> AST
makeLawDecl [] idents
		= Idents idents
makeLawDecl (Ident nm:tail) idents
		= makeLawDecl tail (nm:idents)
makeLawDecl (Idents nms:tail) idents
		= makeLawDecl tail (nms++idents)
makeLawDecl [Type t] idents
		= TypeDecl $ zip idents (repeat $ Just t)

mergeTypeDecl	:: [AST] -> [(Name, Maybe Type)] -> AST
mergeTypeDecl [] acc
		= TypeDecl acc
mergeTypeDecl (TypeDecl stuff:tail) acc
		= mergeTypeDecl tail (stuff++acc)
mergeTypeDecl (Idents nms:tail) acc
		= mergeTypeDecl tail $ zip nms (repeat Nothing) ++acc

pt2law	:: ParseTree -> Law
pt2law	= pt2a h t s convert
