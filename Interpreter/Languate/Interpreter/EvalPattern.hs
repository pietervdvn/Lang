module Languate.Interpreter.EvalPattern where

import StdDef

import Languate.Value
import Languate.TAST
import Languate.Interpreter.BuiltInValues

import Data.Map as M

import Debug.Trace

evalPattern	:: Evaluators -> Context -> Value -> TPattern -> Maybe (Map Name Value)
evalPattern f ctx v (TAssign nm)
		= Just $ M.singleton nm v
evalPattern f ctx v (TMulti pats)
		= pats & mapM (evalPattern f ctx v) |> M.unions
evalPattern _ _ _ TDontCare
		= Just M.empty
evalPattern f ctx v (TDeconstruct sign patterns)
		= do	let err		= error "Hello from evalPattern! How did you get this msg?"
			let texpr		= TApplication err (TCall err sign) $ TLocalCall "patternValue" err
			let (ADT i tp mtuple)	= evalExpr' f ctx {localScope = M.singleton "patternValue" v} texpr	-- we expect a maybe of tuples
			if i == 0 then Nothing else do
			let [tuple]	= mtuple
			let args	= untuple tuple
			if length args /= length patterns then error "Number of patterns and values do not match!" else do
			scopes	<- mapM (uncurry $ evalPattern f ctx) $ zip args patterns
			return $ M.unions scopes
evalPattern _ _ _ pat
		= todos $ show pat
