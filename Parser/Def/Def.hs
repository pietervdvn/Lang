module Def.Def where

import StdDef
{--

This module implements the data structures representing a parsed module.

The data flow is:

String -> parsetree -> ast (per bnf-module) -> here defined data structures -> Cleaned up ADT's/CORE (typechecked/type-inferred/disambiguated functions) -> Interpreter

The structures here include comments (except those withing expressions); the data structures preserve order. These data structures are thus a good starting point for doc generation (but you'll have to sugar a little again, e.g. NormalType List -> [a], a:b:c:#empty -> [a,b,c] ...

--}


type Comment	= String
-- a comment just before any declaration, (thus with no newlines in between)
type DocString	= Comment



data Expression	= Nat Int
		| Flt Float
		| Chr Char
		| Seq [Expression]
		| Tuple [Expression]
		| BuiltIn String	-- Calls a built-in function, e.g. 'toString',
		| Cast Type	| AutoCast
		| Call String
		| Operator String
	deriving (Show)


data Pattern	= Assign

data Type	= Normal String	-- A normal type, e.g. Bool
		| Free String	-- A 'free' type, such as 'a', 'b'. (e.g. in id : a -> a)
		| Applied Type [Type]
		| Curry [Type]
		| TupleType [Type]
		| Infer
	deriving (Show)

data Law	= Law Name Expression Expression
		| Example Expression (Maybe Expression)
