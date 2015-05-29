module Languate.Interpreter.BuiltInValues where

{--
Some builtin values which are used by default throughout the interpreter
--}
import StdDef
import Languate.TAST
import Languate.Value

import Debug.Trace

falseVal	= ADT 0 ([boolType],[]) []
trueVal		= ADT 1 ([boolType],[]) []

nothingVal	= ADT 0 ([maybeType], []) []
justVal	cont	= ADT 1 ([maybeType], []) [cont]

voidVal		= ADT 0 ([voidType], []) []

tupleVal	:: [Value] -> Value
tupleVal conts	= ADT 0 ([tupleType], []) conts

-- builds a normalized tuple for given values
tupleVals	:: [Value] -> Value
tupleVals []	= voidVal
tupleVals [val]	= val
tupleVals (val:vals)
		= tupleVal [val, tupleVals vals]


untuple		:: Value -> [Value]
untuple val@(ADT i (tp, tpreqs) args)
 | val	== voidVal	= []
 | (tp |> isTupleType & or)
			= head args : untuple (last args)
 | otherwise		= [val]


isTupleType	:: RType -> Bool
isTupleType (RApplied rt _)
		= isTupleType rt
isTupleType rt	= rt == tupleType
