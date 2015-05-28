module Languate.Interpreter where

{--
Re-exports all needed functions, with dependency injection
--}
import StdDef
import Languate.Value
import Languate.TAST
import Languate.Interpreter.EvalExpr as EvalExpr
import Languate.Interpreter.EvalPattern as EvalPat

import Data.Map

evaluators	:: Evaluators
evaluators	= Evaluators Languate.Interpreter.evalExpr Languate.Interpreter.evalPattern


evalExpr	:: Context -> TExpression -> Value
evalExpr	= EvalExpr.evalExpr evaluators

evalPattern	:: Context -> Value -> TPattern -> Maybe (Map Name Value)
evalPattern	= EvalPat.evalPattern evaluators
