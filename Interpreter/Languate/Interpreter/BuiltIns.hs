module Languate.Interpreter.BuiltIns where

{--

This module implements '''' apply' '''', a wrapper around '''' apply '''' (which should be passed in explicitly)

apply' has a few hardcoded function names which does special stuff. If no builtin is found, apply' searches functions in the context and  expands them. 

--}
import StdDef
import Languate.AST
import Languate.Signature
import Languate.InterpreterDef
import Languate.Interpreter.Utils

import Data.Maybe

-- apply with search and builtin dispatching. Gives error if function not found
apply'	:: (Value -> Value -> RC Value) -> Signature -> Value -> RC Value
apply' apply (Signature "#fromADT" _) adt@(ADT i _ vals)
	= do	let vals'	= TupleVal $ (ADT i (Normal "Nat") []):vals
		return $ ADT 1 (Applied (Normal "Maybe") [typeOfValue vals'] ) [vals']
apply' apply (Signature "Just" _) arg
	= return $ ADT 1 (Applied (Normal "Maybe") [typeOfValue arg]) [arg]
apply' apply (Signature "Nothing" _) arg
	= return $ ADT 0 (Applied (Normal "Maybe") [typeOfValue arg]) [arg]
apply' apply (Signature "#asTuple" _) arg
	= return $ TupleVal [arg]
apply' apply sign arg
	= do	func	<- searchGlobal' sign
		let func'	= fromMaybe (error $ "Function not found: "++show sign) func
		apply func' arg


isBuiltin	:: Name -> Bool
isBuiltin nm	=  '#' == head nm
