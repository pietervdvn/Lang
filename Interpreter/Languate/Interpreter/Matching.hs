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

-- match takes a value and a pattern, returning possible a binding (if the value matches)
match	:: (Value -> Value -> RC Value) -> Value -> TPattern -> RC (Maybe Binding)
match apply v (TAssign n)	= return $ Just [(n,v)]
match apply v (TDeconstruct funcSign pats)
	= deconstr apply v funcSign pats
match apply v (TMulti pats)
	= do	bindings	<- mapM (match apply v) pats
		if any isNothing bindings then return Nothing
			else return $ Just $ concat $ catMaybes bindings

match apply _ TDontCare	= return $ Just []
match apply (ADT i (Normal "Nat") []) (TEval (TNat j))
	= if i == j then return $ Just [] else return Nothing
match apply v (TEval _)
	= todos "match: TEval for comlex cases"


deconstr :: (Value -> Value -> RC Value) -> Value -> Signature -> [TPattern] -> RC (Maybe Binding)
deconstr apply arg fsign pats
	= do	result	<- apply' apply fsign arg
		let extracted	= extractJust result
		if isNothing extracted then return Nothing
			else split apply (fromJust extracted) pats


-- expects a tuple value to split against the patterns (or a single value)
split	:: (Value -> Value -> RC Value) -> Value -> [TPattern] -> RC (Maybe Binding)
split apply (TupleVal vals) pats
	= do	bindings	<- mapM (\(v,p) -> match apply v p) $ zip vals pats
		if any isNothing bindings then return Nothing
			else return $ Just $ concat $ catMaybes bindings
split apply val [pat]
	= match apply val pat
split _ val pats
	= error $ "Invalid deconstruct: incorrect number of of returned values vs patterns (vals: "++ show val ++ " pats: " ++ show pats ++ ")"



-- returns the clauses which have a result. 
matches	:: (Value -> Value -> RC Value) -> [TClause] -> Value -> RC [(TClause, Binding)]
matches apply clauses value
	=  do	binds	<- mapM (\c -> matches' apply c value) clauses
		return $ catMaybes binds

matches'	:: (Value -> Value -> RC Value) -> TClause -> Value -> RC (Maybe (TClause, Binding))
matches' _ (TClause [] _) _
	= return Nothing
matches' apply (TClause (p:pts) e) v
	=  do	binding	<- match apply v p
		return $ repack (TClause pts e, binding)



extractJust	:: Value -> Maybe Value
extractJust (ADT 1 (Applied (Normal "Maybe") _) [v])
		= Just v
extractJust _	= Nothing
