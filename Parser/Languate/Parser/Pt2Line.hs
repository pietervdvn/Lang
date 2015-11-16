module Languate.Parser.Pt2Line (pt2line) where

import StdDef
import Bnf
import Bnf.ParseTree (ParseTree)
import Languate.Parser.Utils
import Languate.Parser.Pt2Expr
import Languate.Parser.Pt2Pattern
import Languate.Parser.Pt2Comment
import Languate.AST

{--

This module converts the ParseTree into a single line (thus patterns + expression)

--}

modName	= "Pt2Line"

pt2line	:: ParseTree -> Clause
pt2line	=  pt2a h t s convert

convert		:: AST -> Clause
convert (Ln pats e)
		=  Clause pats e
convert ast	=  convErr modName ast


data AST	= Pattern Pattern
		| Patterns [Pattern]
		| Expr Expression
		| Ln [Pattern] Expression
		| NlT (Maybe Comment)
		| Tail [AST]
		| Equals
	deriving (Show)


h		:: [(Name, ParseTree -> AST)]
h		=  [("patternRoot", Pattern . pt2pattern), ("expr", Expr . pt2expr), ("nltab", NlT . pt2nl)]

t		:: Name -> String -> AST
t _ "="		=  Equals
t nm cont	=  tokenErr modName nm cont


s		:: Name -> [AST] -> AST
s "patterns" asts
		= Patterns $ map (\(Pattern p) -> p) asts
s r (pats:NlT cont:Equals:tail)
		= s r (pats:Equals:NlT cont:tail)
s r (NlT cont:Equals:tail)
		= s r (Equals:NlT cont:tail)
s _ tail@(NlT cont:_)
		= Tail tail
s _ (Patterns pats: Equals:tail)
		= Ln pats $ Seq $ getExprs tail
s _ (Equals:tail)
		= Ln [] $ Seq $ getExprs tail
s _ [ast]	= ast
s nm asts	= seqErr modName nm asts

getExprs	:: [AST] -> [Expression]
getExprs [Expr e]
		= [e]
getExprs (Expr e:tail)
		= e:getExprs tail
getExprs (NlT cont:tail)
		= ExpNl cont:getExprs tail
getExprs (Tail tail:rtail)
		= getExprs (tail++rtail)
getExprs asts	= convErr "Pt2Line-getExprs" asts
