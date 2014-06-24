module Languate.Parser.Pt2Function (pt2func) where

import StdDef
import Bnf.ParseTree hiding (Line)
import Bnf
import Languate.Parser.Utils
import Languate.Parser.Pt2Type
import Languate.Parser.Pt2Expr
import Languate.Parser.Pt2Pattern
import Languate.Parser.Pt2Comment
import Languate.Parser.Pt2Law
import Languate.Parser.Pt2Declaration
import Languate.Parser.Pt2Line
import Languate.AST

{--

This module converts the ParseTree into a function declaration, with laws etc.
Declarations may have multiple (explicit) types

--}

modName	= "Pt2Function"

pt2func	:: ParseTree -> ([Comment],Function)
pt2func	=  pt2a h (tokenErr modName) s convert . cleanAll ["nl"]

convert		:: AST -> ([Comment], Function)
convert ast@(Root(Comm comms:_))
		= (init' comms,conv ast $ Function "" [] [] [])

conv		:: AST -> Function -> Function
conv (LineT clause)
		= addClause clause
conv (Laws laws)	
		= conv $ Root $ map LawAst laws
conv (LawAst law)	
		= addLaw law
conv (Decl typ)
		= addDecl typ
conv (Decls decls)
		= conv $ Root $ map Decl decls
conv (Comm [])	= id
conv (Comm comms)
		= setDocStr (last comms)
conv (Root asts)
		= \func -> foldr (\ast func -> conv ast func) func asts
	

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
h		=  [("nls", Comm . pt2nls),("law", LawAst . pt2law),("example", LawAst . pt2law),("declaration", Decl . pt2decl), ("clause",LineT . pt2line)]

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


