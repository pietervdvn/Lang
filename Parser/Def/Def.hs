module Def.Def where

import StdDef
{--

This module implements the data structures representing a parsed module. It is possible to recreate the source code starting from a datastructure like this (module some whitespace that's moved)

The data flow is:

String -> parsetree -> ast (per bnf-module) -> here defined data structures -> Cleaned up ADT's/CORE (typechecked/type-inferred/disambiguated functions) -> Interpreter

The structures here include comments (except those withing expressions); the data structures preserve order. These data structures are thus a good starting point for doc generation (but you'll have to sugar a little again, e.g. NormalType List -> [a], a:b:c:#empty -> [a,b,c] ...

--}


data Function	= Function DocString [(Name, Type)] [Law] [Clause]
	deriving (Show)

data Clause	= Clause [Pattern] Expression
	deriving (Show)

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
		| ExpNl (Maybe Comment)	-- a newline in the expression, which might have a comment 
	deriving (Show)


data Pattern	= Assign Name	-- gives a name to the argument
		| Let Name Expression	-- evaluates the expression, and assigns it to the given name
		| Deconstruct Name [Pattern]	{- deconstructs the argument with a function of type a -> Maybe (d,e,f,...), in which a is of the type of the corresponding argument. The patterns then match the tuple (there should be exactly the same number). If the maybe returns Nothing, matching fails
 Map/Set/List syntactic sugar is translated into something like this. A constructor works in the opposite way here-}
		| Multi [Pattern]	-- at-patterns
		| Eval Expression	-- evaluates an expression, which should equal the argument. Matching against ints is (secretly) done with this. 
		| DontCare		-- underscore
		| MultiDontCare		-- star
	deriving (Show)

{- Deconstruct "unprepend" 
unprepend	: {a} -> (a,{a})
unprepend	: {a --> b} -> ( (a,b), {a --> b} )
unprepend dict	= ( head dict, tail dict)

thus, a pattern like

{a --> b, c : tail} gets translated to
Deconstruct "unprepend" [ Deconstruct "id" [Assign "a", Assign "b"],  Deconstruct "unprepend" [ Deconstruct "id" [Assign "c", Dontcare], Assign "tail"]
                          ^ splitting of key       ^ key        ^ value ^ deconstruct of tail                                 ^about the value  ^the actual tail, after 2 unprepends

-}

data Type	= Normal String	-- A normal type, e.g. Bool
		| Free String	-- A 'free' type, such as 'a', 'b'. (e.g. in id : a -> a)
		| Applied Type [Type]
		| Curry [Type]
		| TupleType [Type]
		| Infer
	deriving (Show)

data Law	= Law Name [(Name, Maybe Type)] Expression Expression
		| Example Expression Expression
	deriving (Show)
