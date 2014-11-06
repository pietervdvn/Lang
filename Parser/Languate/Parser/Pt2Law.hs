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
convert (DiffLaw nm declarations reqs e1 e2)
		= Law nm declarations reqs e1 $ toTrue e2
convert(SimpleLaw nm e1 e2)
		= Example nm e1 $ toTrue e2
convert ast	= convErr "Pt2Law" ast

toTrue		:: Maybe Expression -> Expression
toTrue		=  fromMaybe (Call "True")


data AST	= Tilde	| Bird	| Colon	| Equals
		| ForAll	| Comma
		| LawName Name
		| Ident Name	| Idents [Name]
		| Type Type [TypeRequirement]
		| TypeDecl [(Name, Maybe Type)] [TypeRequirement]	-- name:Type; name:TypeRequirement
		| Expr Expression
		| RightExpr Expression
		-- simplelaw: has no declarations, thus easy for the interpreter
		| DiffLaw (Maybe Name) [(Name, Maybe Type)] [TypeRequirement] Expression (Maybe Expression)	-- name; declaration:  name:Type; type constraints: name:TypeRequirement, expr1, expr2
		| SimpleLaw (Maybe Name) Expression (Maybe Expression)
	deriving (Show)


h		:: [(Name, ParseTree -> AST)]
h		=  [("expr", Expr . pt2expr),("type", uncurry Type . pt2type)]

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
		= mergeTypeDecl asts [] []
s "law" [typeDecls, ForAll]
		= typeDecls
s "law" (LawName name : TypeDecl decls reqs : Expr e1 :maybeRightExpr)
		= DiffLaw (Just name) decls reqs e1 $ rightExpr maybeRightExpr
s "law" (LawName name : Expr e1 : maybeRightExpr)
		= SimpleLaw (Just name) e1 $ rightExpr maybeRightExpr
s "law" (Bird : Expr e1 : maybeRightExpr)
		= SimpleLaw Nothing e1 $ rightExpr maybeRightExpr
s "law" (Bird : TypeDecl decls reqs : Expr e1 : maybeRightExpr)
		= DiffLaw Nothing decls reqs e1 $ rightExpr maybeRightExpr
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
makeLawDecl [Type t reqs] idents
		= TypeDecl (zip idents $ repeat $ Just t) reqs

mergeTypeDecl	:: [AST] -> [(Name, Maybe Type)] -> [TypeRequirement] -> AST
mergeTypeDecl [] acc reqsAcc
		= TypeDecl acc reqsAcc
mergeTypeDecl (TypeDecl stuff reqs :tail) acc reqsAcc
		= mergeTypeDecl tail (stuff++acc) (reqs ++ reqsAcc)
mergeTypeDecl (Idents nms:tail) acc reqsAcc
		= mergeTypeDecl tail (zip nms (repeat Nothing) ++acc) reqsAcc

pt2law	:: ParseTree -> Law
pt2law	= pt2a h t s convert
