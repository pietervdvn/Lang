module Bnf.Meta.Pt2FQN where

{--
This module implements the Pt of "a.b.c.Module" -> FQN conversion
--}
import Control.Monad.Writer
import StdDef
import Bnf.ParseTree
import Bnf.Converter
import Bnf.FQN

data AST	= PName Name
		| Part [Name] Name
		| Dot
	deriving (Show)
parseFQN	:: ParseTree -> Writer Errors FQN
parseFQN pt	=  do 	conved	<- simpleConvert h t s pt
			return $ conv conved

conv	:: AST -> FQN
conv (Part names name)
	= FQN names name


h	:: Name -> ParseTree -> Maybe a
h _ _	= Nothing

t	:: Name -> String -> AST

t "globalIdent" name	= Part [] name
t "packName" name	= PName name
t _ "."			= Dot

s			:: Name -> [AST] -> AST
s "moduleName" [PName head, Dot, Part tail name]
			= Part (head:tail) name
s "moduleName" [ast]	= ast

