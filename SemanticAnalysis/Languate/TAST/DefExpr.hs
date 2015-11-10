module Languate.TAST.DefExpr where

{--
This module implements definitions of the expressions
--}

import StdDef
import Languate.TAST.DefType

-- The typed expression
data TExpression
	=
	{- The TApplication represents the first expression, applied on the second as argument.
		As multiple implementations with the same types exist, multiple types could be returned.
		A TApplication however representats only one of those, and selects those TExpressions which makes it possible.
		This way, a typed expression, has only one possible TypeUnion and one implementation to choose from. -}
	TApplication CTypeUnion TExpression TExpression
	-- we save the type independently as not to change the signature - we need it to look up the implementation
	| TCall CTypeUnion Signature
	| TLocalCall Name CTypeUnion
	deriving (Eq)
