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

import Control.Monad.Reader
import Control.Arrow (first)
import Data.Tuple (swap)

defaultFunctions	= Funcs apply eval

-- in case no arguments are needed
eval	:: Value -> RC Value
eval (VCall ctx sgn)
	= do	found	<- setContext ctx $ search $ sgn
		if isJust found then	eval $ fromJust found
			else error $ "No value found in context for "++ show sgn++show ctx
eval (Lambda [] rt [(ctx, TClause [] expr@(TApplication [t] (TCall [] "#asADT") args))])
	= do	(ADT i _ vals)	<- setContext ctx $ evalExpr multApply expr
		return $ ADT i t vals
eval (Lambda [] rt [(ctx, TClause [] expr)])
	= setContext ctx $ evalExpr multApply expr
eval (Lambda [] rt clauses)
	= eval $ Lambda [] rt $ [head clauses]
eval adt@(ADT _ _ _)	= return adt
eval v	= todos $ "Evaluate (without arg) "++show v


-- applies of the function the given argument. Functions should get passed through 'expand' first, to get rid of VCall's
apply	:: Value -> Value -> RC Value
apply (ADT _ _ _) _
	= error "Application of an argument against a data value is not allowed"
apply (TupleVal _) _
	= error "Application of an argument against a tuple value is not allowed"
apply vcall@(VCall _ _) _
	= error $ "all arguments should be passed through 'expand' first. This is a bug (" ++ show vcall++")"
apply (Lambda [] retTp clauses) arg	= error $ "Application of an argument on a function which doesn't need an extra argument: "++ (show $ map snd clauses)++"; arg is "++show arg
apply (Lambda (argTp:argTps) retTp clauses) arg
	= do	clausesBind'	<- matches defaultFunctions clauses arg	-- returns clauses where the pattern match didn't fail; these clauses need one arg less and carry their context
		ctx		<- ask
		let clauses'	= map (first  (\bind -> setBindings' bind ctx) . swap) $ clausesBind'
		let res		= normalizeLambda $ Lambda (argTps) retTp clauses'
		return res

select	:: Show a => [a] -> a
select []	= error "No match found"
select (a:as)	= a

multApply	:: [Value] -> Value -> RC Value
multApply args (VCall _ (Signature "#asTuple" _))
		= return $ TupleVal args
multApply ((ADT i (Normal "Nat") []):args) (VCall _ (Signature "#asADT" _))
		= return $ ADT i (error $ "What type???") args
multApply [] f	= eval f
multApply (arg:args) vcall@(VCall ctx sign)
		=  setContext ctx $ do	fval	<- apply' defaultFunctions sign arg
					multApply args fval
multApply (arg:args) f
		=  do	val	<- apply f arg
			multApply args val
