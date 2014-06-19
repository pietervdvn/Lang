module Def.Pt2Law (pt2law) where

import StdDef
import Bnf.ParseTree
import Bnf
import Bnf.Converter hiding (convert)
import Control.Monad.Writer
import Control.Monad

import Def.Pt2Prelude
import Def.Pt2Type
import Def.Pt2Expr
import Def.Pt2Pattern
import Def.Def

{--

This module converts the ParseTree into a law/example

--}

convert		:: AST -> Law
convert (SimpleLaw nm declarations e1 e2)
		= Law nm declarations e1 e2
convert(BirdExample e1 e2)
		= Example e1 e2
convert ast	= error $ "Function Convert law fallthrough! "++show ast


data AST	= Tilde	| Bird	| Colon	| Equals
		| ForAll	| Comma
		| LawName Name	
		| Ident Name	| Idents [Name]
		| Type Type
		| TypeDecl [(Name, Maybe Type)]
		| Expr Expression
		| SimpleLaw Name [(Name, Maybe Type)] Expression Expression
		| BirdExample Expression Expression
	deriving (Show)


h		:: StdDef.Name -> ParseTree -> Maybe (Writer Errors AST)
h "expr"	=  Just . fmap Expr . pt2expr
h "type"	=  Just . fmap Type . pt2type
h _		=  const Nothing

hook		:: (ParseTree -> AST) -> ParseTree -> Maybe (Writer Errors AST)
hook f		=  Just . return . f

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
t nm cont	=  error $ "Pt2Expr: Token fallthrough for rule '"++nm++"' with content '"++cont++"'"


s		:: Name -> [AST] -> AST
s _ [Colon, ast]= ast
s _ [Comma, ast]= ast
s "lawDeclaration" asts
		=  makeLawDecl asts []
s "lawDeclarations" asts
		= mergeTypeDecl asts []
s "law" [typeDecls, ForAll]
		= typeDecls
s "law" [Tilde, LawName name, Colon, TypeDecl decls, Expr e1, Equals, Expr e2]
		= SimpleLaw name decls e1 e2
s "law" [Tilde, LawName name, Colon, Expr e1, Equals, Expr e2]
		= SimpleLaw name [] e1 e2
s "example" [Bird, Expr e1, Equals, Expr e2]
		= BirdExample e1 e2
s _ [expr]  	= expr
s nm exprs	= error $ "Pt2Expr: Sequence fallthrough for rule '"++nm++"' with content "++show exprs


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
		= mergeTypeDecl tail $ (zip nms $ repeat Nothing	) ++acc

pt2ast	:: ParseTree -> Writer Errors AST
pt2ast	=  simpleConvert h t s

pt2law	:: ParseTree -> Writer Errors Law
pt2law	pt	= do	ast	<- pt2ast pt
			return $ convert ast
