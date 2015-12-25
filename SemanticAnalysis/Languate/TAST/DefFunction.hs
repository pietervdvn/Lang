module Languate.TAST.DefFunction where

{--
Function related values, e.g. pattern matches, clauses, ...
--}
import StdDef
import Exceptions
import Languate.CheckUtils

import Languate.TAST.DefType
import Languate.TAST.TypeUtils
import Languate.TAST.DefExpr
import Languate.TAST.ExprUtils


import Languate.AST

import Data.Map

{-
A local scope keeps track of what variable has what type.
It is build based on the pattern matching of the function, while typing the patterns.
-}
type LocalScope	= Map Name [RType]	-- Variable a is **all** of the given types. Requirements might apply, but will be known in context
data TPattern	= TAssign Name
		| TDeconstruct Signature [TPattern]
		| TMulti [TPattern]
		| TDontCare
		| TEval TExpression	-- The value should be the same as the result of this expression
	deriving (Show, Eq)

data TClause		= TClause [TPattern] TExpression
	deriving (Show, Eq)
type FuncBody	= [TClause]




getVisibility	:: Module -> Visible -> Signature -> Exc Visible
getVisibility mod vis sign
	= do	let nm	= signName sign
		let restrictions	= exports mod
		let publ	= isPublic vis
		let publ'	= isAllowed restrictions nm
		-- the function declares it as private, but the whitelist exports it
		errIf (isWhitelist restrictions && not publ && publ')
			("Conflicting visibilities for "++nm++", it was declared privatly, but exported in the module header")
		return $ toVisible (publ && publ')
