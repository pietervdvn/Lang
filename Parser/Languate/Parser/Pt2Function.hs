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
import Data.Maybe (fromMaybe)

import Debug.Trace
{--

This module converts the ParseTree into a function declaration, with laws etc.
Declarations may have multiple (explicit) types

--}

modName	= "Pt2Function"

pt2func	:: ParseTree -> ([Comment],Function)
pt2func	=  pt2a h (tokenErr modName) s convert



convert		:: AST -> ([Comment], Function)
convert ast	= let (Root asts)	= normalize ast in
		  let commAsts		= filter isComment asts in
		  let comms		= filter (/= "") $ concatMap (\(Comm c) -> c) commAsts in
			(init' comms, conv (Root asts) $ Function "" Public [] [] [])

conv		:: AST -> Function -> Function
conv (LineT clause)
		= addClause clause
conv (LawAst law)
		= addLaw law
conv (Decl typ)
		= addDecl typ
conv (Comm [])	= id
conv (Comm comms)
	| last comms /= ""
		= setDocStr (last comms)
	| otherwise
		= conv (Comm $ init comms)
conv (Root asts)
		= \func -> foldr conv func asts


preComms	:: [AST] -> [Comment]
preComms	=  init' . concatMap (\(Comm c) -> c) . filter isComment

data AST	= Decl (Name, Type, Visible, [TypeRequirement])
		| LawAst Law
		| Comm [Comment]
		| LineT Clause
		| Root [AST]
	deriving (Show)


h		:: [(Name, ParseTree -> AST)]
h		=  	[ ("nl"		, Comm	 . (:[]) . fromMaybe "" . pt2nl)
		   	, ("nls"	, Comm   . pt2nls)
			, ("law"	, LawAst . pt2law)
			, ("declaration", Decl   . pt2decl)
			, ("clause"	, LineT  . pt2line)]

s _ [ast]	= ast
s _ asts	= Root asts


normalize	:: AST -> AST
normalize ast	=  Root $ unflatten ast

unflatten	:: AST -> [AST]
unflatten (Root asts)
		= concatMap unflatten asts
unflatten ast	= [ast]

isComment	:: AST -> Bool
isComment (Comm _)
		= True
isComment _	= False
