module Languate.Interpreter.BuiltInValues where

{--
Some builtin values which are used by default throughout the interpreter
--}

import Languate.TAST
import Languate.Value

falseVal	= ADT 0 ([boolType],[]) []
trueVal		= ADT 1 ([boolType],[]) []

nothingVal	= ADT 0 ([maybeType], []) []
justVal	cont	= ADT 1 ([maybeType], []) [cont]

voidVal		= ADT 0 ([voidType], []) []

-- builds a normalized tuple for given values
tupleVals	:: [Value] -> Value
tupleVals []	= voidVal
tupleVals [val]	= val


untuple		:: Value -> [Value]
untuple val
 | val	== voidVal	= []
 | otherwise		= [val]
