module Bnf.Meta.Pt2Regex where

import StdDef
import Bnf.BNF hiding (Star, And)
import Bnf.ParseTree
import Bnf.Converter
import Control.Monad.Writer
import qualified Regex.Def as R
import Normalizable

{--
This module implements the function to make a regex from a pt
--}

data AST	= Sequence [AST]
		| SubR AST
		| AndR AST AST
		| OrR AST AST
		| RangeChar Char
		| RangeUnitRange Char Char
		| NotRgx AST
		| Range [AST]
		| FixedChar Char
		| Times Int Int AST
		| TimesToken Int Int
		| MinTimes Int AST
		| MinTimesToken Int
		| Dot
		| Star	| Plus	| Quest
		| RangeOpen	| RangeClose
		| ParO		| ParC
		| AccO		| AccC
		| Not	| And	| Or
		| Int Int
		| Comma
	deriving (Show)

parseRegex	:: ParseTree -> Writer Errors R.Regex
parseRegex pt	= do	conved	<- simpleConvert (\_ _ -> Nothing) t s pt
			return $ normalize $ conv conved	

conv		:: AST -> R.Regex
conv (FixedChar c)	= R.Fixed c
conv (RangeChar c)	= R.Fixed c
conv (RangeUnitRange c1 c2)
			= R.Range c1 c2
conv (Range ranges)	= R.Or $ map conv ranges
conv (AndR r1 r2)	= R.And [conv r1, conv r2]
conv (OrR r1 r2)	= R.Or [conv r1, conv r2]
conv (NotRgx r)		= R.Invert $ conv r
conv (Times i j ast)	= R.BetweenTimes i j $ conv ast
conv (MinTimes i ast)	= R.MinTimes i $ conv ast
conv (Sequence asts)	= R.Seq $ map conv asts
conv (SubR ast)		= R.Seq [conv ast]

t	:: Name -> String -> AST

t "rangeUnitChar" "\\t"	= RangeChar '\t'
t "rangeUnitChar" "\\n"	= RangeChar '\n'
t "rangeUnitChar" "\\f"	= RangeChar '\f'
t "rangeUnitChar" "\\"	= RangeChar '"'
t "rangeUnitChar" str	= RangeChar $ head str

t "normalChar" ('\\':[c])
			= FixedChar c
t "normalChar" (c:[])	= FixedChar c

t "metaChar" "\\t"	= FixedChar '\t'
t "metaChar" "\\n"	= FixedChar '\n'
t "metaChar" "\\f"	= FixedChar '\f'

t "int" str		= Int $ read str

t "not" _	= Not
t "and" _	= And
t "or" _	= Or

t _ "["		= RangeOpen
t _ "]"		= RangeClose
t "dot" _	= Dot
t _ "("		= ParO
t _ ")"		= ParC
t _ "{"		= AccO
t _ "}"		= AccC
t _ ","		= Comma
t _ "*"		= Star
t _ "+"		= Plus
t _ "?"		= Quest


t name _	= error $ "Token fallthrough for "++name


s	:: Name -> [AST] -> AST
s "rangeUnit" [RangeChar c1, Dot, Dot, RangeChar c2]
	= RangeUnitRange c1 c2
s "range" (RangeOpen:r@(Range{}):RangeClose:[])
	= r
s "range" items
	= Range items

s "pass1" [ParO,rgx,ParC]
			= SubR rgx
s "pass2" [Not, ast]	= NotRgx ast
s "pass2" [ast]		= ast

s "pass3" [ast]		= ast
s "pass3" [ast, Star]	= MinTimes 0 ast
s "pass3" [ast, Plus]	= MinTimes 1 ast
s "pass3" [ast, Quest]	= Times 0 1 ast
s "pass3" [ast, TimesToken i j]	
			= Times i j ast
s "pass3" [ast, MinTimesToken i]	
			= MinTimes i ast
s "times" [AccO, Comma, AccC]
			= MinTimesToken 0
s "times" [AccO, Int i, Comma, AccC]
			= MinTimesToken i
s "times" [AccO, Comma, Int i, AccC]
			= TimesToken 0 i
s "times" [AccO, Int i, Comma, Int j, AccC]
			= TimesToken i j
s "times" [AccO, Int i, AccC]
			= TimesToken i i

s "sequence" [ast]	= ast
s "sequence" asts	= Sequence asts

s "regex" (ast:And:rest)	= AndR ast $ s "regex" rest
s "regex" (ast:Or:rest)		= OrR ast $ s "regex" rest
s _ [ast]		= ast


s name ast	= error $ "Sequence fallthrough for "++name++ " with asts "++ show ast


