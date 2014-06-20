module Def.Parser.Pt2Fuction (pt2func) where

import StdDef
import Bnf.ParseTree
import Bnf
import Control.Monad.Writer
import Control.Monad
import Def.Parser.Utils
import Def.Parser.Pt2Prelude
import Def.Parser.Pt2Type
import Def.Parser.Pt2Expr
import Def.Parser.Pt2Pattern
import Def.Def

{--

This module converts the ParseTree into a function declaration, with laws etc.
Declarations may have multiple (explicit) types

--}

pt2func	:: ParseTree -> Writer Errors Expression
pt2func	pt	= do	ast	<- pt2ast pt
			return $ convert ast

convert		:: AST -> Expression
convert ast	= error $ "Function Convert fallthrough! "++show ast


data AST	= Nil
	deriving (Show)


h		:: StdDef.Name -> ParseTree -> Maybe (Writer Errors AST)
h _		=  const Nothing

hook		:: (ParseTree -> AST) -> ParseTree -> Maybe (Writer Errors AST)
hook f		=  Just . return . f

t		:: Name -> String -> AST
t nm cont	=  error $ "Pt2Expr: Token fallthrough for rule '"++nm++"' with content '"++cont++"'"


s		:: Name -> [AST] -> AST
s _ [expr]  = expr
s nm exprs	= error $ "Pt2Expr: Sequence fallthrough for rule '"++nm++"' with content "++show exprs


pt2ast	:: ParseTree -> Writer Errors AST
pt2ast	=  simpleConvert h t s

