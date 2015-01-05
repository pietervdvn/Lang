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

This module converts the ParseTree into a function declaration.
Declarations may have multiple (explicit) types.
--}

modName	= "Pt2Function"

pt2func	:: ParseTree -> Function
pt2func	=  pt2a h (tokenErr modName) s convert



convert		:: AST -> Function
convert ast	=  conv (normalize ast) $ Function Public [] []

conv		:: AST -> Function -> Function
conv (LineT clause)
		= addClause clause
conv (Decl typ)
		= addDecl typ
conv (Root asts)
		= \func -> foldr conv func asts


data AST	= Decl (Name, Type, Visible, [TypeRequirement])
		| LineT Clause
		| Root [AST]
	deriving (Show)


h		:: [(Name, ParseTree -> AST)]
h		=  	[ ("declaration", Decl   . pt2decl)
			, ("clause"	, LineT  . pt2line)]

s _ [ast]	= ast
s _ asts	= Root asts


normalize	:: AST -> AST
normalize ast	=  Root $ unflatten ast

unflatten	:: AST -> [AST]
unflatten (Root asts)
		= concatMap unflatten asts
unflatten ast	= [ast]
