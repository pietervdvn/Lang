module Languate.Interpreter.Matching where

{---
Pattern matching of a value against a pattern. Produces a binding.

Each function can return only one value (of one type).
This assumption might be wrong, but will be coded against in this version of the interpreter.
--}

import StdDef
import Languate.Interpreter.Utils
import Languate.Interpreter.BuiltIns
import Languate.InterpreterDef
import Languate.Signature
import Languate.TAST
import Languate.AST
import Data.Maybe
import Control.Arrow

import Debug.Trace

-- match takes a value and a pattern, returning possible a binding (if the value matches)
match	:: Funcs -> Value -> TPattern -> RC (Maybe Binding)
match fs v (TAssign n)	= return $ Just [(n,v)]
match fs v (TDeconstruct funcSign pats)
	= deconstr fs v funcSign pats
match fs v (TMulti pats)
	= do	bindings	<- mapM (match fs v) pats
		if any isNothing bindings then return Nothing
			else return $ Just $ concat $ catMaybes bindings

match apply _ TDontCare	= return $ Just []
match apply (ADT i (Normal "Nat") []) (TEval (TNat j))
	= if i == j then return $ Just [] else return Nothing
match apply v (TEval _)
	= todos "match: TEval for comlex cases"


deconstr :: Funcs -> Value -> Signature -> [TPattern] -> RC (Maybe Binding)
deconstr fs arg fsign pats
	= do	result	<- apply' fs fsign arg >>= (evalFunc fs)
		let extracted	= extractJust result
		if isNothing extracted then return Nothing
			else split fs (fromJust extracted) pats


-- expects a tuple value to split against the patterns (or a single value)
split	:: Funcs -> Value -> [TPattern] -> RC (Maybe Binding)
split fs (TupleVal vals) pats
	= do	bindings	<- mapM (\(v,p) -> match fs v p) $ zip vals pats
		if any isNothing bindings then return Nothing
			else return $ Just $ concat $ catMaybes bindings
split fs val [pat]
	= match fs val pat
split _ val pats
	= error $ "Invalid deconstruct: incorrect number of of returned values vs patterns (vals: "++ show val ++ " pats: " ++ show pats ++ ")"



-- returns the clauses which have a result. 
matches	:: Funcs -> [(Context, TClause)] -> Value -> RC [(TClause, Binding)]
matches fs clauses value
	=  do	binds	<- mapM (\(ctx, c) -> setContext ctx $ matches' fs c value) clauses
		return $ catMaybes binds

matches'	:: Funcs -> TClause -> Value -> RC (Maybe (TClause, Binding))
matches' _ (TClause [] _) _
	= return Nothing
matches' apply (TClause (p:pts) e) v
	=  do	binding	<- match apply v p
		curBind	<- ask' bindings
		return $ fmap (second (++curBind)) $ repack (TClause pts e, binding)



extractJust	:: Value -> Maybe Value
extractJust (ADT 1 (Applied (Normal "Maybe") _) [v])
		= Just v
extractJust _	= Nothing
