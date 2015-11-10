module Haskell where

{--

This module implements

--}


{-
A local scope keeps track of what variable has what type.
It is build based on the pattern matching of the function, while typing the patterns.
-}
type LocalScope	= Map Name RType
data TPattern	= TAssign Name
		| TDeconstruct Signature [TPattern]
		| TMulti [TPattern]
		| TDontCare
		| TEval TExpression	-- The value should be the same as the result of this expression
	deriving (Show, Eq)

data TClause		= TClause [TPattern] TExpression
	deriving (Show, Eq)
type FuncBody	= TClause
