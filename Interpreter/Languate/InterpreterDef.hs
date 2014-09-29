module Languate.InterpreterDef where

{--

This module implements all data definitions for the interpreter.

--}

import StdDef
import Languate.TypedPackage
import Languate.FQN
import Languate.AST
import Languate.TAST
import Languate.SymbolTable
import Languate.Signature
import Data.List (intercalate)

import Control.Monad.Reader



-- The context in which an expression can be evaluated.
data Context	= Context { world :: TPackage, country :: FQN, bindings :: Binding}
type RC	a	= Reader Context a

type Binding	= [(Name, Value)]


data Value	= ADT Int Type [Value]
		| TupleVal [Value]
		| VCall Signature	-- A call to a function. The exact function should be declared using the signature.
		| Lambda [Type] Type [TClause]	-- Application which failed to evaluate to a simple value. The [Type] is what is still expected, where as the second lonely type is the returned type with completed evaluation.



instance Show Value where
	show	= sv

sv	:: Value -> String
sv (ADT i t vals)
	= "ADT: "++show i++" <"++show t++"> " ++ show vals
sv (VCall (Signature name _))
	= name
sv (Lambda _ _ tClause)
	=  "LAMBDA: " ++ show tClause

typeOfValue		:: Value -> Type
typeOfValue (ADT _ t _)	= t
typeOfValue (TupleVal vals)
			= TupleType $ map typeOfValue vals
typeOfValue (VCall (Signature _ t))	= t
typeOfValue (Lambda args ret _)	= Curry $ args++[ret]
