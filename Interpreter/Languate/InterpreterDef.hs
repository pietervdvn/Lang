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
data Context	= Context { world :: TPackage, country :: FQN, bindings :: SymbolTable Value}
type RC	a	= Reader Context a

-- one name can be bound to multiple types, by dynamic invoked functions.
type Binding	= [(Name, [Value])]


data Value	= ADT Int Type String	-- The string is a print representation
		| Tuple [Value]
		| VCall Signature	-- A call to a function. The exact function should be declared using the signature.
		| App [TClause] Value	-- Application of the given args on the given clauses. Note that some or all clauses can fail	
		| Lambda TClause	-- Application which failed to evaluate to a simple value



instance Show Value where
	show	= sv

sv	:: Value -> String
sv (ADT i t str)
	= str
sv (VCall (Signature name _))
	= name
sv (App clauses v)
	= "APP: "++ intercalate "\\" (map show clauses) ++ show v
sv (Lambda tClause)
	=  "LAMBDA: " ++ show tClause
