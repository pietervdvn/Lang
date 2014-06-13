module Def.Pt2Expr where

import StdDef
import Bnf.ParseTree
import Bnf
import Bnf.Converter
import Control.Monad.Writer

{--

This module converts the ParseTree (of an expression) into a Expression, via an AST

--}

data AST	= Nil
	deriving (Show)


t		:: Name -> String -> AST
t nm cont	=  error $ "Pt2Expr: Token fallthrough for rule '"++nm++"' with content '"++cont++"'"


s		:: Name -> [AST] -> AST
s _ [expr]  = expr
s nm exprs	= error $ "Pt2Expr: Sequence fallthrough for rule '"++nm++"' with content "++show exprs

pt2ast	:: ParseTree -> Writer Errors AST
pt2ast	=  simpleConvert (const $ const Nothing) t s
