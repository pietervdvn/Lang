module Def.Parser.Pt2Function (pt2func) where

import StdDef
import Bnf.ParseTree hiding (Line)
import Bnf
import Control.Monad.Writer
import Control.Monad
import Def.Parser.Utils
import Def.Parser.Pt2Prelude
import Def.Parser.Pt2Type
import Def.Parser.Pt2Expr
import Def.Parser.Pt2Pattern
import Def.Parser.Pt2Comment
import Def.Parser.Pt2Law
import Def.Parser.Pt2Declaration
import Def.Parser.Pt2Line
import Def.AST

{--

This module converts the ParseTree into a function declaration, with laws etc.
Declarations may have multiple (explicit) types

--}

modName	= "Pt2Function"

pt2func	:: ParseTree -> ([Comment],Function)
pt2func	=  pt2a h t s convert . cleanAll ["nl"]

convert		:: AST -> ([Comment], Function)
convert (Root asts)
		= (preComms asts, conv asts)
convert ast	=  convErr modName ast

conv		:: [AST] -> Function
conv []		=  Function "" [] [] []
conv (LineT clause:tail)
		= let Function docstr types laws clauses	= conv tail in
			Function docstr types laws (clause:clauses)
conv (Laws ls:tail)	
		= let Function docstr types laws clauses = conv tail in
			Function docstr types (ls++laws) clauses
conv (LawAst law:tail)	
		= let Function docstr types laws clauses = conv tail in
			Function docstr types (law:laws) clauses
conv (Decl typ:tail)
		= let Function docstr types laws clauses = conv tail in
			Function docstr (typ:types) laws clauses
conv (Decls typs:tail)
		= let Function docstr types laws clauses = conv tail in
			Function docstr (typs++types) laws clauses
conv (Comm comms:tail)
		= let Function _ types laws clauses  = conv tail in
			Function (last comms) types laws clauses
conv asts	= convErr (modName++"-conv") (Root asts)
	

preComms	:: [AST] -> [Comment]
preComms	=  init' . concatMap (\(Comm c) -> c) . filter isComment

data AST	= Decl (Name, Type)
		| Decls [(Name,Type)]
		| LawAst Law
		| Laws [Law]
		| Comm [Comment]
		| LineT Clause
		| Root [AST]
	deriving (Show)


h		:: [(Name, ParseTree -> AST)]
h		=  [("nlcomment", Comm . inLst . pt2comment),("law", LawAst . pt2law),("example", LawAst . pt2law),("declaration", Decl . pt2decl), ("clause",LineT . pt2line)]

t		:: Name -> String -> AST
t nm cont	=  tokenErr modName nm cont


s		:: Name -> [AST] -> AST
s _ comms@(Comm _:Comm _:_)
		= Comm $ accComms comms
s _ lws@(LawAst _:_)
		= Laws $ accLaws lws
s _ lws@(Laws _:_)
		= Laws $ accLaws lws
s _ decls@(Decl _:_)
		= Decls $ accDecls decls
s _ decls@(Decls _:_)
		= Decls $ accDecls decls
s _ [ast]	= ast
s _ asts	= Root asts


accLaws		:: [AST] -> [Law]
accLaws	[]	= []
accLaws (LawAst l:tail)
		=  l:accLaws tail
accLaws (Laws ls:tail)
		= ls++accLaws tail

accDecls	:: [AST] -> [(Name,Type)]
accDecls []	= []
accDecls (Decl l:tail)
		=  l:accDecls tail
accDecls (Decls ls:tail)
		= ls++accDecls tail

accComms	:: [AST] -> [Comment]
accComms []	= []
accComms (Comm c:tail)
		= c++accComms tail

isComment	:: AST -> Bool
isComment (Comm _)
		= True
isComment _	= False


