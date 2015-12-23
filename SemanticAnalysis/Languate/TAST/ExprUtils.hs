module Languate.TAST.ExprUtils where

import StdDef
import HumanUtils
import Languate.TAST.DefExpr
import Languate.TAST.DefType
import Languate.TAST.TypeUtils


{-Smashes the argument into the function, without actually checking the types.
Only use for pregenerated functions
-}
simpleApply	:: TExpression -> TExpression -> TExpression
simpleApply func arg
	= let	(tps, reqs)	= typeOf func
		tp'	= (tps |> dropCurry, reqs)
		in
		TApplication tp' func arg

instance Show TExpression where
	show 	= showTE

-- Typed Expression to string
showTE	:: TExpression -> String
showTE (TApplication (retTps, reqs) func arg)
	= let	funcStr	= pars (show func)
		argStr	= pars (show arg)
		tpStr	= show retTps in
		funcStr ++ " " ++ argStr ++ " :"++tpStr
showTE (TCall _ sign)
	= signName sign
showTE (TLocalCall nm _)
	= show nm
showTE (Tag t)
	= "§"++show t


typeOf		:: TExpression -> CTypeUnion
typeOf (TCall typeInfo _)
		= typeInfo
typeOf (TApplication typeInfo _ _)
		= typeInfo
typeOf (TLocalCall _ t)
		= t
