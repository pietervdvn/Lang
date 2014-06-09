module Bnf.Meta.Pt2BnfExpr where

import Regex
import StdDef
import Bnf.ParseTree
import Bnf.Converter
import Control.Monad.Writer
import qualified Bnf.Meta.Pt2Regex as R
import qualified Bnf.BNF as B

{--
This module implements the convert methods
-}

parseExpr	:: ParseTree -> Writer Errors B.Expression
parseExpr pt	=  do	conved	<- simpleConvert h t s pt
			return $ conv conved


conv		:: AST -> B.Expression
conv (SubExpr ast)	= conv ast
conv (Choice asts)	= B.Choice $ map conv asts
conv (Set asts)		= B.Set $ map conv asts
conv (Sequence asts)	= B.Seq $ map conv asts
conv (Rgx regex)	= B.Rgx regex
conv (Ident name)	= B.Call name
conv (RuleCall call)	= B.Call call
conv (Plus ast)		= B.More $ conv ast
conv (Quest ast)	= B.Opt $ conv ast
conv (Star ast)		= B.Star $ conv ast
conv ast		= error $ "Could not convert "++ show ast


data AST	= SubExpr AST
		| Choice [AST]
		| Set [AST]
		| SetItem AST
		| Sequence [AST]
		| Rgx Regex
		| Ident Name
		| RuleCall Name
		| Plus AST	| Quest AST	| Star AST
		| PlusT	| QuestT| StarT
		| ParO	| ParC
		| AccO	| AccC
		| DQuote
		| Bar	| NewLine
	deriving (Show)


h	:: Name -> ParseTree -> Maybe (Writer Errors AST)
h "regex" pt	= Just (do	rgx	<- R.parseRegex pt
				return $ Rgx rgx)
h _ _		= Nothing

t	:: Name -> String -> AST

t "localIdent" ident	= Ident ident
t _ "\""	= DQuote
t _ "("		= ParO
t _ ")"		= ParC
t _ "{"		= AccO
t _ "}"		= AccC
t "or" _	= Bar
t _ "?"		= QuestT
t _ "+"		= PlusT
t _ "*"		= StarT
t _ "\n\t"	= NewLine


t name str	= error $ "Token fallthrough for "++name++ " " ++ show str


s	:: Name -> [AST] -> AST

s "expression" [seq]	= seq
s "expression" [seq1, Bar, seq2]
			= Choice [seq1, seq2]
s "expression" (Choice heads:tails)
			= let Choice tail	= s "expression" tails in
				Choice $ heads++tail


s "sequence" terms	= Sequence terms

s "factor" [term]	= term
s "factor" [term, PlusT]
			= Plus term
s "factor" [term, QuestT]
			= Quest term
s "factor" [term, StarT]
			= Star term

s "term" [Ident name]	= RuleCall name
s "term" [Bar, end]	= SetItem end
s "term" [SetItem head]	= Set [head]
s "term" (SetItem head:tails)
			= let (Set tail)	= s "term" tails in
				Set $ head:tail
s "term" [AccO, head, Set tail, AccC]	= Set $ head:tail
s "term" [ParO, expr, ParC]	= SubExpr expr

s "term" [DQuote, r@(Rgx{}), DQuote]	= r
s "or" [Bar, Bar]	= Bar
s _ [ast]	= ast
s name ast	= error $ "Sequence fallthrough for "++name++ " with asts "++ show ast


