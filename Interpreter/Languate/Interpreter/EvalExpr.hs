module Languate.Interpreter.EvalExpr where

{--
This module implements the expression evaluator.
--}
import StdDef
import Languate.InterpreterDef
import Languate.TAST
import Languate.AST
import Languate.Signature
import Data.Char
import Languate.Interpreter.Utils
import Languate.Interpreter.BuiltIns

import Data.Maybe
import Control.Monad.Reader
import Debug.Trace

-- evaluates an expression. The function is a workaround for the cyclic import problem. The multi-apply function should be dropped in there
evalExpr	:: ([Value] -> Value -> RC Value) -> TExpression -> RC Value
evalExpr _ (TNat i)
		= return $ ADT i (Normal "Nat") []
evalExpr _ (TFlt i)
		= return $ todo "Float exprEval"
evalExpr _ (TChr c)
		= return $ ADT (ord c) (Normal "Char") []
evalExpr apply (TApplication ts expr exprs)
		= do	args	<- mapM (evalExpr apply) exprs
			if isBuiltinTCall expr then builtinAppl apply expr args
				else do	func	<- evalExpr apply expr
					apply args func
-- if the name is locally known, use the local variable; if not convert to a VCall (delegate lookup to semantal)
evalExpr _ (TCall possTypes name)
		= do	localVar	<- searchLocal name
			ctx		<- ask
			if isJust localVar then return $ fromJust localVar
				else do	if not $ isBuiltin name then return $ VCall ctx (Signature name $ selectT name possTypes)
						 	else return $ VCall ctx (Signature name Infer)


builtinAppl	:: ([Value] -> Value -> RC Value) -> TExpression -> [Value] -> RC Value
builtinAppl multApply (TCall _ nm) args
	= do	let sign = Signature nm $ error $ "Builtins should not be dependant on the type! Tried to call type of "++show nm
		ctx	<- ask
		multApply args (VCall ctx sign)

selectT	:: Name -> [Type] -> Type
selectT n []	= error $ "No possible type for TCall. This is a bug (prob. missing builtin, perhaps "++n++")"
selectT _ [t]	= t
selectT _ ts	= head ts


isBuiltinTCall	:: TExpression -> Bool
isBuiltinTCall (TCall _ n)
		= isBuiltin n
isBuiltinTCall _	= False
