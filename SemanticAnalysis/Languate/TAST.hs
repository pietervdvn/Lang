module Languate.TAST where

{--

This module the TypeChecked-AST, the 'simpler' version of Languate.AST as all source-code things, docs, ... are stripped of. Only the program remains, to be interpreted later.

It is this data-structure that all semantic analysis things use.

--}

import StdDef
import Languate.AST

data TypedExpression	= TNat Int	| TFlt Float	| TChr Char	-- primitives
			{- the first argument, [Type] are all the possible **return** types. E.g. '(&&) True False' -> Call [Bool] "&&" [..., ...]; '(&&) True' -> Call [Bool -> Bool] -}
			| TApplication [Type] TypedExpression [TypedExpression]
			| TCall [Type] Name	
	deriving (Show)
type TExpression	= TypedExpression

-- TODO eval is left out for a later version
data TPattern	= TAssign Name
		| TDeconstruct Name [Pattern]
		| TMulti [TPattern]
		| TDontCare
	deriving (Show)

data TClause		= TClause [TPattern] TExpression
	deriving (Show)
