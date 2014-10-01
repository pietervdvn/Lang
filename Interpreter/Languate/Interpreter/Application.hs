module Languate.Interpreter.Application where

{-
Interpreter.Application handles the application of arguments. 
Apply takes a value and an argument, and produces a new value, the one representing the result.
Might give an error
-}
import StdDef
import Languate.AST
import Languate.TAST
import Languate.InterpreterDef
import Languate.Interpreter.Matching
import Languate.Interpreter.EvalExpr
import Languate.Interpreter.Utils
import Languate.Interpreter.EvalExpr
import Languate.Interpreter.BuiltIns
import Languate.Signature
import Data.Maybe

import Debug.Trace

-- in case no arguments are needed
eval	:: Value -> RC Value
eval (VCall n t)
	= todos "Eval search"
eval (Lambda ctx [] rt [TClause [] expr@(TApplication [t] (TCall [] "#asADT") args)])
	= do	(ADT i _ vals)	<- evalExpr multApply expr
		return $ ADT i t vals
eval (Lambda ctx [] rt [TClause [] expr])
	= evalExpr multApply expr


-- applies of the function the given argument. Functions should get passed through 'expand' first, to get rid of VCall's
apply	:: Value -> Value -> RC Value
apply (ADT _ _ _) _
	= error "Application of an argument against a data value is not allowed"
apply (TupleVal _) _
	= error "Application of an argument against a tuple value is not allowed"
apply vcall@(VCall _ _) _
	= error $ "all arguments should be passed through 'expand' first. This is a bug (" ++ show vcall++")"
apply (Lambda ctx argTps rt clauses) arg
	= do	clauses'	<- matches apply clauses arg	-- returns each clause in its binding which still exists
		
		
apply (Lambda ctx [t] rt clauses) arg	-- this is the last argument in the chain
	= do	clauses'	<- matches apply clauses arg
		let (clause, binding)	=  select clauses'	-- use the first clause
		bindings	<- ask' bindings
		let (TClause _ e) = clause
		setBindings (binding++bindings) $ evalExpr multApply e
apply lambda@(Lambda ctx [] rt [clause]) arg
		= trace ("Unused argument: "++show arg) $ eval lambda
apply v v2	= error $ "Missed a case in apply: "++show v++" <> "++show v2		



select	:: [a] -> a
select []	= error "No match found"
select (a:as)	= a

multApply	:: [Value] -> Value -> RC Value
multApply args (VCall _ (Signature "#asTuple" _))
		= return $ TupleVal args
multApply ((ADT i (Normal "Nat") []):args) (VCall _ (Signature "#asADT" _))
		= return $ ADT i (error $ "What type???") args
multApply [] f	= return f
multApply (arg:args) vcall@(VCall ctx sign)
		=  setContext ctx $ do	fval	<- apply' apply sign arg
					multApply args fval
multApply (arg:args) f
		=  do	val	<- apply f arg
			multApply args val
