module Languate.Interpreter where

{--

This module implements the interpreter. 

The most important function is ''eval'' which does all the work, together with ''match'', which matches values against patterns and builds the binding.
--}


{-

eval reduces the expression as much as possible.
It is guaranteed not to be an ''App''. Thunks are possible, if not enough arguments are given
-}
import StdDef
import Data.Maybe

import Languate.AST
import Languate.TAST
import Languate.InterpreterDef
import Languate.Interpreter.Utils
import Languate.Signature

import Data.Char (ord)

import Control.Monad.Reader


-- evaluates as much as possible. The type is the requested type
eval	:: Value -> Type -> Reader Context Value
eval (Lambda clauses) reqT
	= do	evaluated 	<- mapM (flip evalClause reqT) clauses
		let evaluated'	= catMaybes evaluated
		if null evaluated' then error ("No value found for "++show clauses)
			else return $ head evaluated'
eval v _	= return v


-- matches a pattern against a value. Might evaluate values
match	:: Value -> TPattern -> RC (Maybe Binding)
match v (TAssign n)
	= return $ Just [(n,[v])]
match v (TDeconstruct sign pats)
	= do	-- there is only a single function which matches a -> Maybe (b,c,...)
		-- This means only a single binding is possible.
		-- (This comment is a lie, but we will assume this for now. It's tracked and should be solved by adding a list with deconstruct) --
		todos "TDeconstruct match"
match v (TMulti pats)
	= do	-- all should match!
		bindings	<- mapM (match v) pats	-- :: [ Maybe Binding]
		return $ if any isNothing bindings
				then Nothing 
				else Just $ mergeBindings $ catMaybes bindings
match v TDontCare
	= return $ Just []
match v (TEval (TNat i))
	= do	val	<- eval v (Normal "Int")
		case val of
			ADT j typ _	-> if i == j && typ `elem` [Normal "Int", Normal "Nat"]
						then return $ Just []
						else return Nothing
			_		-> return Nothing
match v (TEval expr)
	= todos "Evaluate expression in interpreter"


-- deconstructs using the given signature. When this function is non-determenistic, an error is thrown.
deconstruct	:: Value -> Signature -> RC (Maybe [Value])
deconstruct v sign
		= do	tclauses'	<- searchGlobal sign
			let err		= error $ ("No deconstruction funtion found with name "++show sign)
			let tclauses	= fromMaybe err tclauses'
			-- let v		<- apply 
			todos "Deconstruct"


{- simple apply applies a single argument to the TClause. 
Assumes at least one pattern is in the clause. 
-}
simpleApply	:: TClause -> Value -> RC (Maybe (Binding, TClause))
simpleApply (TClause [] e) _
		= error ("Applied to too many arguments: "++show e)
simpleApply (TClause (pat:pats) expr) v
		= do	bind	<- match v pat
			if isNothing bind then return Nothing
			else do	return $ Just (fromJust bind, TClause pats expr)

simpleApplies	:: TClause -> [Value] -> RC (Maybe (Binding, TClause))
simpleApplies cl args
		= _simpleApplies cl args []


_simpleApplies	:: TClause -> [Value] -> Binding -> RC (Maybe (Binding, TClause))
_simpleApplies cl (arg:args) prevBind
		= do	applied	<- simpleApply cl arg
			if isNothing applied then return Nothing
			else do	let (bind, clause)	= fromJust applied
				_simpleApplies clause args (prevBind ++ bind)

apply		:: [TClause] -> [Value] -> RC Value
apply clauses args
		= do	bindClauses	<- mapM (flip _simpleApplies args) clauses
			let matching	=  catMaybes bindClauses
			if null matching then error "No matching clause found"
			else do	let (binding, TClause [] expr)	= head matching
				let st	= stFromList binding
				setBindings st $ evalExpr expr (todo)


-- The type is requested type. If a choice is possible, the given type is used to dispatch a TCall.
evalExpr	:: TExpression -> Type -> RC Value
evalExpr (TNat i) _
		=  return $ ADT i (Normal "Int") []
evalExpr (TFlt f) _
		= todos "Value for float in evalExpr"
evalExpr (TChr c) _
		= return $ ADT (ord c) (Normal "Char") []
evalExpr (TApplication [t] (TCall [] "#asADT") (TNat i:contents)) _
		= do	contents'	<- mapM (flip evalExpr $ todos "Search for constructor needed types") contents
			return $ ADT i t contents'
evalExpr tapp@(TApplication ts texpr exprs) reqT
	| reqT `elem` ts	
		= do	let needed	= neededArgs (typeOf texpr) reqT
			args		<- mapM (uncurry evalExpr) $ zip exprs needed
			todos $ "evalExpr "++ show texpr ++" $ " ++show args



	| otherwise	= error $ "Unsolvable Tapplication: expected type "++show reqT++"; available types: "++show ts
evalExpr (TCall ts n) reqT
	| reqT `elem` ts	
			= do	let sign	= Signature n reqT
				search sign >>= flip eval reqT
	| otherwise	= error $ "Unsolvable TCall: expected type "++show reqT++"; available types: "++show ts
				
-- evaluates a TClause as much as possible. Returns nothing if more pattern matching has to happen
evalClause	:: TClause -> Type -> RC (Maybe Value)
evalClause (TClause [] texpr) reqT
		= fmap Just $ evalExpr texpr reqT
evalClause _ _	= return Nothing

