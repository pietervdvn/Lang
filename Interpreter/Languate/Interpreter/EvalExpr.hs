module Languate.Interpreter.EvalExpr where

import StdDef
import Languate.Value
import Languate.TAST
import Languate.TableOverview
import Languate.FunctionTable
import Languate.BuiltIns
import Languate.Interpreter.BuiltInValues

import Data.Map as M
import Data.List as L
import Data.Maybe


-- Eval to WHNF
evalExpr	:: Evaluators -> Context -> TExpression -> Value
evalExpr f ctx (TLocalCall nm tpinf)
	= let 	errMsg	=  "Local variable "++show nm++" not found" in
		ctx & localScope & findWithDefault (printStackTrace errMsg ctx) nm
-- builtin function to check constructors
evalExpr f ctx (TApplication _
		(TApplication _ (TCall _ (Signature{signName="is"})) number)	-- function which is called (check)
			valExpr)							-- argument
	= let	exp	= evalNatValue f ctx number
		(ADT i typ _)	= evalExpr f ctx valExpr in
		if exp == i then trueVal else falseVal
-- deconstructtion of a value
evalExpr f ctx (TApplication _
		(TApplication _ (TCall _ (Signature{signName="deconstruct"})) number)	-- function which is called (check)
			valExpr)							-- argument used in the constructor
	= let 	exp	= evalNatValue f ctx number
		(ADT i typ args)	= evalExpr f ctx valExpr in
		if exp /= i then nothingVal else	-- deconstructor failed, not the right constructor
			justVal $ tupleVals args
-- construct a new value
evalExpr f ctx (TApplication typ (TCall _ (Signature{signName = "construct"})) i)
	= ADT (evalNatValue f ctx i) typ []
evalExpr f ctx tapp@(TApplication typeInf func arg)
	= apply' f ctx func arg
evalExpr f ctx (TCall _ sign)
	= let	fqn	= signFQN sign
		-- the new context in which the clause should be evaluated
		newStack = show sign : stack ctx
		ctx'	= ctx {location = fqn, localScope = M.empty, stack = newStack }
		errMsg	= "No implementation table found for "++ show fqn
		impTbl	= ctx & package & implementations & unpackITS &
				M.findWithDefault (printStackTrace errMsg ctx') fqn & imps
		errMsg'	= "No implementation found for "++show sign++" within context "++show (location ctx')++" and local scope "++show (localScope ctx')
		clauses	= impTbl & M.findWithDefault (printStackTrace errMsg' ctx') sign
		ctxStacked	= ctx {stack = newStack}	in
		evalThunk f $ Thunk $ zip (repeat ctxStacked) clauses


-- extracts the integer value of encoded, natural values as (Succ (Succ Zero))
evalNatValue	:: Evaluators -> Context -> TExpression -> Int
evalNatValue f ctx texpr@(TApplication (_,_) succ a)
	| succ == natTypeSucc
		= 1 + evalNatValue f ctx a
	| otherwise
		= _errMsg texpr
evalNatValue f ctx texpr
	| texpr == natTypeZero
		= 0
	| otherwise
		= _errMsg texpr


_errMsg texpr	= error $ "This is not a natural value: "++show texpr++"(internal error in evalNatValue, probably an abused constructor function or something like that; file a bug)"

-- When a thunk is ready for unpacking (no more needed args in head), it unpacks
evalThunk	:: Evaluators -> Value -> Value
evalThunk evals (Thunk ((ctx, TClause [] texpr):_))
		= evalExpr' evals ctx texpr
evalThunk _ v	= v

apply'		:: Evaluators -> Context -> TExpression -> TExpression -> Value
apply' f ctx texpr arg
		= let	valFun	= evalExpr f ctx texpr
			valArg	= evalExpr f ctx arg in
			apply f ctx valFun valArg

-- TODO typecheck to much patterns!
apply		:: Evaluators -> Context -> Value -> Value -> Value
apply f ctx (ADT i (types, typeReqs) args) arg
		= ADT i (types |> dropTypeCurry ctx, typeReqs) (args++[arg])
apply f ctx (Thunk clauses) arg
		= let clauses'	= clauses |> uncurry (evalClauses f arg) & catMaybes in
			if L.null clauses' then printStackTrace "Pattern fallthrough. No clauses match!" ctx
			else evalThunk f $ Thunk clauses'

dropTypeCurry	:: Context -> RType -> RType
dropTypeCurry _ (RCurry _ tp)
		= tp
dropTypeCurry ctx t
		= printStackTrace ("Could not drop a curry type from: "++show t++", probably a ADT which is applied to too much arguments") ctx


evalClauses	:: Evaluators -> Value -> Context -> TClause -> Maybe (Context, TClause)
evalClauses f arg ctx (TClause (pat:pats) expr)
	= do	scope	<- evalPattern' f ctx arg pat
		let ctx'=  ctx {localScope = M.union scope $ localScope ctx}
		return (ctx', TClause pats expr)
