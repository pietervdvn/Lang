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

instance Eq Context where
	(==) (Context _ fqn bnds) (Context _ fqn' bnds')
		= fqn == fqn' && bnds == bnds'

data Funcs	= Funcs {applyFunc :: Value -> Value -> RC Value, evalFunc :: Value -> RC Value}	-- needed for dependency-injection, as haskell does not support cyclic imports
type RC	a	= Reader Context a

type Binding	= [(Name, Value)]


data Value	= ADT Int Type [Value]
		| TupleVal [Value]
		| VCall Context Signature	-- A call to a function. The exact function should be declared using the signature.
		| Lambda [Type] Type [(Context, TClause)]	-- Application which failed to evaluate to a simple value. The [Type] is what is still expected, where as the second lonely type is the returned type with completed evaluation. Each clause has his own context, as local bindings might differ from previous data binds
	deriving (Eq)


instance Show Value where
	show	= sv

sv	:: Value -> String
sv (ADT i t vals)
	= "ADT: "++show i++" <"++show t++"> " ++ show vals
sv (VCall _ (Signature name _))
	= "CALL: " ++  show name
sv (Lambda argT retT tClause)
	=  "LAMBDA: <"++show argT++"> -> <"++show retT++">" ++ (show $ map snd tClause)
sv (TupleVal vals)
	= "TUPLE "++ show vals

instance Show Context where
	show	= sc

sc ctx	= "Context: @"++(show $ country ctx)++" ; locally known: "++(show $ map fst $ bindings ctx)

typeOfValue		:: Value -> Type
typeOfValue (ADT _ t _)	= t
typeOfValue (TupleVal vals)
			= TupleType $ map typeOfValue vals
typeOfValue (VCall _ (Signature _ t))	= t
typeOfValue (Lambda args ret _)	= Curry $ args++[ret]

setBindings'	:: Binding -> Context -> Context
setBindings' b (Context w c _)
		= Context w c b

normalizeLambda	:: Value -> Value
normalizeLambda (Lambda argTypes (Curry curryTps) clauses)
	= Lambda (argTypes ++ init' curryTps) (last curryTps) clauses
normalizeLambda l	= l
