module Languate.Interpreter.EvalPattern where

import StdDef

import Languate.Value
import Languate.TAST

import Data.Map as M

evalPattern	:: Evaluators -> Context -> Value -> TPattern -> Maybe (Map Name Value)
evalPattern f ctx v (TAssign nm)
		= Just $ M.singleton nm v
evalPattern f ctx v (TMulti pats)
		= pats & mapM (evalPattern f ctx v) |> M.unions
evalPattern _ _ _ TDontCare
		= Just M.empty
evalPattern _ _ _ pat
		= todos $ show pat
