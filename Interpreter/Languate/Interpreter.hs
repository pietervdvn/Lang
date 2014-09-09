module Languate.Interpreter where

{--

This module implements a simple interpreter for languate.

It takes a map (FQN -> SimpleSymbolTable), where it finds the implementations.

--}

import StdDef
import Languate.AST
import Languate.TAST
import Languate.TypedPackage
import Languate.FQN
import Languate.Signature
import Languate.SymbolTable
import Languate.InterpreterDef
import Control.Monad.Reader
import qualified Data.Map as M
import Data.Maybe
import Normalizable
import Languate.PatternTypeChecker (getDeconstructType)

import Languate.Interpreter.Utils

-- RC = Reader Context; context contains the code world

{-- reduces the given once as much as possible. More or less normalize a, except for the context.
Postconditions:
The given values are either 
-> Primitive
-> ADT
-> VCall
-> Thunk (if a Thunk was passed in)

It is never
-> App
-> SubstitutionFrontier
---}
eval	:: Value -> RC [Value]
eval app@(App ts v [])
	= return [app]
eval (App ts vcall@(VCall _ _ _) args)
	=  do	expands	<- expand vcall
		evals	<- mapM (eval . flip wrapAsApp args) expands
		return $ concat evals
eval (App ts val (arg:args))
	= do	thunks	<- eval val
		valss	<- mapM (\thunk -> applyOnThunk thunk arg) thunks
		todos $ "vals " ++ (show $ concat valss)
		


eval v@(Primitive _)	=  return [v]
eval v@(ADT _ _ _)	=  return [v]
eval v@(VCall _ _ _)	=  return [v]
eval v@(Thunk _ _ _)	=  return [v]
eval (SubstitutionFrontier v)
			=  eval v

applyOnThunk	:: Value -> Value -> RC [Value]
applyOnThunk (Thunk fqn ts clauses) arg
	= do	applied <- mapM (apply fqn arg) clauses
		return $ concat applied

-- applies the argument to the clause if possible. If matching failed, an ampty list will be returned.
apply	:: FQN -> Value -> TClause -> RC [Value]
apply _ v (TClause [] expr)
	= return []
apply fqn v (TClause (pt:pts) expr)
	= do	bindings	<- matchPattern fqn v pt
		todos "Apply"
		

{- tries to match a pattern against the given argument. If this is not possible (no pattern match available), you'll receive a nothing.
Multiple deconstructors might match, this is why a list is returned
-}
matchPattern	:: FQN -> Value -> TPattern -> RC [Binding]
matchPattern _ _ TDontCare
		= return []
matchPattern _ (Primitive (TNat i)) (TEval (TNat j))
		= return $ if i == j then [[]] else []
matchPattern _ _ (TEval _)
		= todos "Interpreter: TEval pattern/equality for real types"
matchPattern _ v (TAssign nm)
		=  return $ [[(nm, v)]]
matchPattern fqn v (TDeconstruct func pats)
		= do	let typesValue	= typeOfValue v
			possibleSignatures	<- mapM (\t -> _searchDF fqn func t (length pats)) typesValue
			let possibleTypes	= map (\(Signature _ t) -> t) $ concat possibleSignatures 
			-- for the vcall (function call) we need the signature, thus all possible types
			thunks	<- expand $ VCall fqn possibleTypes func
			let apps	= map (flip wrapAsApp [v]) thunks
			results	<- fmap concat $ mapM eval apps
			let matches	= filter (not . isNothingV) results
			return $ error $ ">>> pattern matches from deconstructor: thunks\n" ++ show thunks ++ "\n>>>apps\n" ++ show apps ++ "\n>>>results\n" ++ show results ++ "\n>>>matches\n" ++show matches



{--
# expands a VCall into a lambda

Only the name is given; this mean we have to figure out what functions we are talking about.
We first take a look at the bindings. If the name of the VCall lives there, that defenition is used.
We lookup all matching signatures + where they live, and spit out all matching functions + typesigns as Lambda's.

These are interpreted in a next step with pattern substitution (if they are applied).

Note that expansion is done as lately as possible, to keep printing pretty.

--}
expand	:: Value -> Binding -> RC [Value]
expand (VCall fqn ts name) binding
	= do	tmod	<- getModule fqn	-- a reference to implementation is in scope, no need to jump following the imports
		let imps	= typedClauses tmod
		let signs	= map (Signature name) ts
		let err		= error $ "Interpreter error: function "++name++" not found"
		let tclausess	= zip signs $ catMaybes $ map (flip lookupSt imps) signs
		let thunks	= map (uncurry $ \sign@(Signature _ t) clauses -> Thunk fqn [t] clauses) tclausess
		return thunks


