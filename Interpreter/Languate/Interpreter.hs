module Languate.Interpreter where

{--

This module implements a simple interpreter for languate.

It takes a map (FQN -> SimpleSymbolTable), where it finds the implementations.

--}

import StdDef
import Languate.TAST
import Languate.TypedPackage
import Languate.FQN
import Languate.Signature
import Languate.SymbolTable
import Languate.InterpreterDef
import Control.Monad.Reader
import qualified Data.Map as M
import Data.Maybe


data Context	= Context {world::TypedPackage}
	deriving (Show)
type RC	a	= Reader Context a


-- reduces the given once as much as possible
eval	:: Value -> RC [Value]
eval app@(App ts v [])
	= return [app]
eval (App ts vcall@(VCall _ _ _) args)
	=  do	expands	<- expand vcall
		evals	<- mapM (\expanded -> eval $ App ts expanded args) expands
		return $ concat evals
eval (App ts (Thunk fqn thunkts clauses) (arg:args))
	= todo
eval v	=  return [v]



-- applies the argument to the clause -if possible-
apply	:: Value -> TClause -> RC (Maybe Value)
apply v (TClause [] expr)
	= return Nothing
apply v (TClause (pt:pts) expr)
	= todo

-- tries to match a pattern against the given argument. If this is not possible (no pattern match available), you'll receive a nothing. If it worked out, you'll get a binding
matchPattern	:: FQN -> Value -> TPattern -> RC (Maybe [(Name, Value)])
matchPattern _ _ TDontCare
		= return $ Just []
matchPattern _ _ (TEval _)
		= todos "Interpreter: TEval pattern/equality"
matchPattern _ v (TAssign nm)
		=  return $ Just [(nm, v)]
matchPattern fqn v (TDeconstruct func pats)
		= do	let typesValue	= typeOfValue v
			let types	= typesValue
			-- for the vcall (function call) we need the signature, thus all possible types
			let vcall	= VCall fqn types func
			return Nothing




{--
# expands a VCall into a lambda

Only the name is given; this mean we have to figure out what functions we are talking about.
We lookup all matching signatures + where they live, and spit out all matching functions + typesigns as Lambda's.

These are interpreted in a next step with pattern substitution (if they are applied).

Note that expansion is done as lately as possible, to keep printing pretty

--}
expand	:: Value -> RC [Value]
expand (VCall fqn ts name)
	= do	tmod	<- getModule fqn	-- a reference to implementation is in scope, no need to jump following the imports
		let imps	= typedClauses tmod
		let signs	= map (Signature name) ts
		let err		= error $ "Interpreter error: function "++name++" not found"
		let tclausess	= zip signs $ catMaybes $ map (flip lookupSt imps) signs
		let thunks	= map (uncurry $ \sign@(Signature _ t) clauses -> Thunk fqn [t] clauses) tclausess
		return thunks



getModule	:: FQN -> RC TModule
getModule fqn	=  do	tpack	<- fmap world $ ask
			let err	= error $ "Interpreter error: location not known: "++show fqn
			return $ fromMaybe err $ M.lookup fqn tpack

