module Def.AST where

import StdDef
{--

This module implements the data structures representing a parsed module. It is possible to recreate the source code starting from a datastructure like this (module some whitespace that's moved)

The data flow is:

String -> parsetree -> ast (per bnf-module) -> here defined data structures -> Cleaned up ADT's/CORE (typechecked/type-inferred/disambiguated functions) -> Interpreter

The structures here include comments (except those withing expressions); the data structures preserve order. These data structures are thus a good starting point for doc generation (but you'll have to sugar a little again, e.g. NormalType List -> [a], a:b:c:#empty -> [a,b,c] ...

--}


{- data ADT	= A | B | C 

A single constructor is represented by a single ADTSum-object.

-}

data Visible	= Private	
		| Public
	deriving (Show)

data ADTSum	= ADTSum Name Visible (Maybe Comment) [(Maybe Name, Type)]
	deriving (Show)

setComment	:: Comment -> ADTSum -> ADTSum
setComment comm (ADTSum nm v _ nmts)
		=  ADTSum nm v (Just comm) nmts

setCommentIf	:: Comment -> ADTSum -> ADTSum
setCommentIf comm sum@(ADTSum _ _ Nothing _)
		= setComment comm sum
setCommentIf _ sum
		= sum
	
-- data List a  = Cons a (List a) | Nil
-- becomes : ADTDef "List" ["a"] "Comment about a list" product
data ADTDef	= ADTDef Name [Name] DocString [ADTSum]
	deriving (Show)


-- e.g. type Name = String
-- no obligated docstring for this one!
data SynDef	= SynDef Name [Name] Type

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
		{- Deconstructs the argument with a function of type a -> Maybe (d,e,f,...), in which a is of the type of the corresponding argument.
			The patterns then match the tuple (there should be exactly the same number). If the maybe returns Nothing, matching fails
 				Map/Set/List syntactic sugar is translated into something like this. A constructor works in the opposite way here-}
		| Deconstruct Name [Pattern]
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


-- restrict is the blacklist/whitelist of the showing/hiding in an import statement
data Restrict	= BlackList [Name] | WhiteList [Name]
	deriving (Show)
-- represents an import statement. public - Path - ModuleName- restrictions
data Import	= Import Visible [Name] Name Restrict
	deriving (Show)
