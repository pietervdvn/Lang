module Languate.InterpreterDef where

{--

This module implements the data structures needed for the interpreter and some usefull functions for those (show, typeOfValue)

--}
import StdDef
import Languate.AST hiding (sp)
import Languate.TAST
import Control.Monad.Reader
import Data.List
import Languate.FQN

-- a value represents an evaluated expression or a thunk. These are typed and carry their location, their context in which they should be evaluated
data Value	= Primitive TExpression		-- primitive value such as nat or float
		| ADT Type Int String		-- an adt-value/constructor. The string is a print representation (of the constructor)
		| VCall FQN [Type] Name		-- function call, the FQN is the fqn of the context where it is called; types+name = signature (thus types should be the expected function types, e.g. [Int -> Int -> Int, Nat -> Nat -> Nat]. This does indeed imply that you should know the possible signatures beforehand. You do know these however, as they are calculated in the typechecker.
		| Thunk FQN [Type] [TClause]	-- expanded function call, with the actual implementation; bound variables are expanded in this version.
		| App [Type] Value [Value]	-- application of the first value with the remaining values


typeOfValue	:: Value -> [Type]
typeOfValue (Primitive tExpr)
		=  typeOf tExpr
typeOfValue (VCall _ t _)	= t
typeOfValue (Thunk _ t _)	= t
typeOfValue (App t _ _)	= t
typeOfValue (ADT t _ _)	= [t]


instance Show Value where
	show	= sv



sv	:: Value -> String
sv (Primitive texp)
	= sp texp
sv (VCall _ _ name)
	= name
sv (Thunk _ _ clauses)
	= "(THUNK: " ++ intercalate " / " (map show clauses) ++ ")"
sv (App _ v vs)
	= "(" ++ show v ++ ")" ++ (unwords $ map show vs)
sv (ADT _ _ nm)
	= nm


sp		:: TExpression -> String
sp (TNat i)	= show i
sp (TFlt f)	= show f
sp (TChr c)	= [c]
