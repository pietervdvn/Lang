module Languate.AST.FunctionAST where

{--

This module implements the Abstract Syntax tree data structures representing functions, patterns, expressions and clauses, thus all kind of things about function definitions.
--}
import StdDef
import Languate.AST.TypeAST

data Function	= Function	{ visibility	:: Visible
				, signs		:: [(Name, Type, [TypeRequirement])]
				, clauses	:: [Clause]
				}

-- (Function docString decls laws clauses)
data Clause	= Clause [Pattern] Expression


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
	deriving (Eq)



{- Deconstructor of collection is done with (:)
(:-	: {a} -> (a,{a})
(:)	: {a --> b} -> ( (a,b), {a --> b} )
dict	= ( head dict, tail dict)

This is implemented into the collection framework.

thus, a pattern like

{a --> b, c : tail} gets translated to
Deconstruct "(:)" [ Deconstruct "id" [Assign "a", Assign "b"],  Deconstruct "(:)" [ Deconstruct "id" [Assign "c", Dontcare], Assign "tail"]
                          ^ splitting of key       ^ key        ^ value ^ deconstruct of tail                                 ^about the value  ^the actual tail, after 2 (:)

-}
