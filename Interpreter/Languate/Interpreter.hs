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

import Control.Monad.Reader

eval	:: Value -> Reader Context [Value]
eval (App clauses arg)
	= fmap concat $ mapM (flip apply arg) clauses 
eval v	= return [v]


apply	:: TClause -> Value -> RC [Value]
apply (TClause (pat:pats) expr)
	= error ""


-- matches a pattern against a value. Might evaluate values
match	:: Value -> TPattern -> RC (Maybe Binding)
match v (TAssign n)
	= return $ Just [(n,[v])]
match v (TDeconstruct nm pats)
match v (TMulti pats)
	= do	-- all should match!
		bindings	<- mapM (match v) pats	-- :: [ Maybe Binding]
		return $ if any isNothing bindings
				then Nothing 
				else Just $ mergeBindings $ catMaybes bindings
match v TDontCare
	= return $ Just []
match v (TEval (TNat i))
	= do	val	<- eval v
		case val of
			ADT j typ _	-> if i == j && typ `elem` [Normal "Int", Normal "Nat"]
						then return $ Just []
match v (TEval expr)
	= todos "Evaluate expression in interpreter"
