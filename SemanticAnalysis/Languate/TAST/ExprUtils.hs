module Languate.TAST.ExprUtils where

import StdDef
import HumanUtils
import Languate.TAST.DefExpr
import Languate.TAST.DefType
import Languate.TAST.TypeUtils

import Control.Arrow


{-Smashes the argument into the function, without actually checking the types.
Only use for pregenerated functions
-}
simpleApply	:: TExpression -> TExpression -> TExpression
simpleApply func arg
	= TApplication (typeOf func & first dropCurry) func arg

instance Show TExpression where
	show 	= showTE

-- Typed Expression to string
showTE	:: TExpression -> String
showTE (TApplication tp func arg)
	= let	funcStr	= show func
		argStr	= show arg
		tpStr	= show tp in
		pars (funcStr ++ " " ++ argStr) -- ++ " :"++tpStr
showTE (TCall _ sign)
	= signName sign
showTE (TLocalCall _ nm)
	= nm
showTE (Tag t)
	= "§"++show t


typeOf		:: TExpression -> CType
typeOf (TCall t _)
		= t
typeOf (TApplication t _ _)
		= t
typeOf (TLocalCall t _)
		= t
