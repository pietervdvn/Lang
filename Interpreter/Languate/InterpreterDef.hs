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
		| VCall Context Signature	-- A call to a function. The exact function should be declared using the signature.
		| Lambda [Type] Type [(Context, TClause)]	-- Application which failed to evaluate to a simple value. The [Type] is what is still expected, where as the second lonely type is the returned type with completed evaluation. Each clause has his own context, as local bindings might differ from previous data binds



instance Show Value where
	show	= sv

sv	:: Value -> String
sv (ADT i t vals)
	= "ADT: "++show i++" <"++show t++"> " ++ show vals
sv (VCall _ (Signature name _))
	= "CALL: " ++  show name
sv (Lambda _ _ tClause)
	=  "LAMBDA: " ++ (show $ map snd tClause)
sv (TupleVal vals)
	= "TUPLE "++ show vals

typeOfValue		:: Value -> Type
typeOfValue (ADT _ t _)	= t
typeOfValue (TupleVal vals)
			= TupleType $ map typeOfValue vals
typeOfValue (VCall _ (Signature _ t))	= t
typeOfValue (Lambda args ret _)	= Curry $ args++[ret]
