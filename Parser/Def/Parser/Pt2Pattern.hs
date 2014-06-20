module Def.Parser.Pt2Pattern (pt2pattern) where

import StdDef
import Bnf
import Bnf.ParseTree

import Control.Monad.Writer
import Control.Monad
import Def.Parser.Utils
import Def.Def 
import Def.Parser.Pt2Expr
import Def.Parser.Pt2Prelude

import Data.Maybe
{--
This module converts parstrees into  patterns
--}


-- rething parsing for the bnf lib
data AST	= Star
		| Underscore
		| Dollar
		| Expr Expression
		| Constructor Name
		| Deconstructor Name
		| Deconstr Name [AST]
		| Deconstructeds [AST]
		| Ident Name
		| AtPart [AST]
		| Minus	| At	| ArrowT
		| SetHead AST (Maybe AST)
		| SetTail AST
		| SetP [AST] [Maybe AST] (Maybe AST)	-- a set/dict pattern. The first list are the keys, second are values, last is tail. E.g. {a --> b, c : d} -> SetP [Ident "a", Ident c] [Just $ Ident b, Nothing] (Just $ Ident "d")
		| SetPCont [AST] [Maybe AST]
		| DictValue AST
		| ListPart AST
		| ListTail AST
		| ListHeads [AST]
		| ListP [AST] (Maybe AST)
		| TuplePart AST
		| TupleP [AST]
		| TupleO | TupleC
		| SetO	| SetC
		| ParO	| ParC
		| ListO	| ListC
		| Comma	| ColonT
	deriving (Show)

h		:: [(Name, ParseTree -> AST)]
h		= [("simpleExpr", Expr . pt2expr),("nat", Expr . Nat . parseNat)]


t		:: Name -> String -> AST
t "globalIdent" name
		=  Constructor name
t "localIdent" name
		=  Ident name
t "ident" name	=  Deconstructor name
t _ "*"		=  Star
t _ "_"		=  Underscore
t _ "$"		=  Dollar
t _ "-"		=  Minus
t "tuple" "("	=  TupleO
t "tuple" ")"	=  TupleC
t _ "{"		=  SetO
t _ "}"		=  SetC
t _ "("		=  ParO
t _ ")"		=  ParC
t _ "["		=  ListO
t _ "]"		=  ListC
t _ "@"		=  At
t _ ","		=  Comma
t _ "-->"	=  ArrowT
t _ ":"		=  ColonT
t name str	=  tokenErr "Pt2Pattern" name str

s		:: Name -> [AST] -> AST
s "patternRoot" [Dollar, Expr e]
		= Expr e
s "patternRoot" [ParO, pattern, ParC]
		= pattern
s "patternRoot" [At, AtPart patterns]
		= AtPart patterns
s "patternRoot" [At, pattern]
		= AtPart [pattern]
s "patternRoot" [ast, AtPart patterns]
		= AtPart (ast:patterns)
s "pattern" [Minus, Expr e]
		= Expr (Seq [Call "-", e])
s "deconstructed" asts
		= Deconstructeds asts
s "pattern" [Deconstructor name, Deconstructeds asts]
		= Deconstr name asts
s "pattern" [Deconstructor name]
		= Deconstr name []
s "pattern" [ast]	= ast
s "pattern" asts
		= Deconstr "" asts
s "dictPart" [ArrowT, ast]
		= DictValue ast
s "dictPart" [ast, DictValue astv]
		= SetHead ast $ Just astv
s "dictPart" [ast]
		= SetHead ast Nothing
s "dict" [Comma, ast]
		= ast
s "dict" [ColonT, ast]
		= SetTail ast
s "dict" (SetO:asts)
		= buildDictAST asts [] [] 
s "list" [Comma, ast]
		= ListPart ast
s "list" [ColonT, ast]
		= ListTail ast
s "list" [ListPart ast]
		= ListHeads [ast]
s "list" (ListPart ast:parts)
		= let ListHeads asts = s "list" parts in
			ListHeads (ast:asts)
s "list" (ListO:ident:asts)
		= let ListP asts' mtail = buildListAST (reverse asts) [] in
			ListP (ident:asts') mtail
s "tuple" [Comma, ast]
		= TuplePart ast
s "tuple" [TuplePart ast]
		= TupleP [ast]
s "tuple" (TuplePart ast:parts)
		= let TupleP tail = s "tuple" parts in
			TupleP (ast:tail)
s "tuple" [TupleO, pattern, TupleP patterns, TupleC]
		= TupleP (pattern:patterns)
s _ [ast]  	= ast
s nm asts	= seqErr "Pt2Pattern" nm asts

buildListAST	:: [AST] -> [AST] -> AST
buildListAST [] heads
		= ListP heads Nothing
buildListAST (ListC:tail) h
		= buildListAST tail h
buildListAST (ListTail ast:tail) h
		= let ListP asts _ = buildListAST tail h in
			ListP asts (Just ast)
buildListAST (ListHeads asts:tail) h
		= buildListAST tail (asts ++ h)

buildDictAST	:: [AST] -> [AST] -> [Maybe AST] -> AST
buildDictAST [] heads headValues
		= SetP (reverse heads) (reverse headValues) Nothing
buildDictAST (SetC:tail) h hv
		= buildDictAST tail h hv
buildDictAST (SetTail ast:tail) h hv
		= let SetP heads headValues _ = buildDictAST tail h hv in
			SetP heads headValues (Just ast)
buildDictAST (SetHead ast mValAst : tail) h hv
		= buildDictAST tail (ast:h) (mValAst:hv)

convert		:: AST	-> Pattern
convert Star	= MultiDontCare
convert Underscore	= DontCare
convert (Expr e)	= Eval e
convert (Ident name)	= Assign name
convert (Constructor name)
			= Deconstruct name []
convert (Deconstr name asts)
			= Deconstruct name $ map convert asts	
convert (AtPart asts)	= Multi $ map convert asts
convert (SetP asts mValAsts mtail)
			= if any isJust mValAsts 
				then convDict asts (map fm mValAsts) mtail
				else convSet asts mtail
				where fm	= fromMaybe Underscore
convert (ListP asts mtail)
		= convList asts mtail
convert (TupleP asts)
		= Deconstruct "just" $ map convert asts
convert ast	= convErr "Pt2Pattern" ast

convList	:: [AST] -> Maybe AST -> Pattern
convList [] (Just tail)
		= convert tail
convList [ast] Nothing
		= Deconstruct "getLonely" [convert ast]
convList (head:heads) mtail
		= Deconstruct "unprepend" [convert head, convList heads mtail]

convDict	:: [AST] -> [AST] -> Maybe AST -> Pattern
convDict [] [] (Just tail)	
			= convert tail
convDict [ast] [vast] Nothing
			= Deconstruct "getLonely" [convert ast, convert vast]
convDict (key:ktail) (val:vtail) t
		=  Deconstruct "unprepend" [Deconstruct "id" [convert key, convert val],
			convDict ktail vtail t]

convSet		:: [AST] -> Maybe AST -> Pattern
convSet [] (Just tail)	=  convert tail
convSet [head] Nothing
		= Deconstruct "getLonely" [convert head]
convSet (head:headtail) tail
		=  Deconstruct "unprepend" [convert head, convSet headtail tail]

pt2pattern	:: ParseTree -> Pattern
pt2pattern	=  pt2a h t s convert
