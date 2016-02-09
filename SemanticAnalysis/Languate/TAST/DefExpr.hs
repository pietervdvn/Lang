module Languate.TAST.DefExpr where

{--
This module implements definitions of the expressions
--}

import StdDef
import Languate.TAST.DefType

{- The typed expression.
	All types ^(except for Tag) have their own RType, the type they return.
		This might be an RConj, or this might be a free on which requirements lay; these are known from the context.

	Tag does not have a type. It is solely used to construct/deconstruct values and will never appear in an untyped clause.
		It is inserted into generated clauses such as " #construct 0 arg arg "


-}
data TExpression
	=
	{- The TApplication represents the first expression, applied on the second as argument.
		As multiple implementations with the same types exist, multiple types could be returned.
		A TApplication however representats only one of those, and selects those TExpressions which makes it possible.
		This way, a typed expression, has only one possible type and one implementation to choose from. -}
	TApplication CType TExpression TExpression
	-- we save the type independently as not to change the signature - we need it to look up the implementation
	| TCall CType Signature
	| TLocalCall CType Name
	| Tag Int	-- The tag is used in combination of builtin functions to construct and deconstruct values
	deriving (Eq)
