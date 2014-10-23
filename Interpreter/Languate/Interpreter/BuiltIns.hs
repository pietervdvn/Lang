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

import  Control.Monad.Reader

-- apply with search and builtin dispatching. Gives error if function not found
apply'	:: Funcs -> Signature -> Value -> RC Value
apply' _ (Signature "#fromADT" _) adt@(ADT i _ vals)
	= do	let vals'	= TupleVal $ ADT i (Normal "Nat") []:vals
		return $ ADT 1 (Applied (Normal "Maybe") [typeOfValue vals'] ) [vals']
apply' funcs sign@(Signature "#fromADT" _) arg	-- arg is not evaluated yet
	= do	adt	<- evalFunc funcs arg
		apply' funcs sign adt
apply' _ (Signature "Just" _) arg
	= return $ ADT 1 (Applied (Normal "Maybe") [typeOfValue arg]) [arg]
apply' _ (Signature "Nothing" _) arg
	= return $ ADT 0 (Applied (Normal "Maybe") [typeOfValue arg]) [arg]
apply' _ (Signature "#asTuple" _) arg
	= return $ TupleVal [arg]
apply' funcs sign arg
	= do	func	<- searchGlobal' sign
		ctx	<- ask
		let func'	= fromMaybe (error $ "Apply' (builtIns): Function not found: "++show sign++" in "++show ctx) func
		applyFunc funcs func' arg


isBuiltin	:: Name -> Bool
isBuiltin nm	=  '#' == head nm
