module Languate.Interpreter where

{--

This module implements a simple interpreter for languate.

It takes a map (FQN -> SimpleSymbolTable), where it finds the implementations.


TODO fix this:
It hopes the expressions and functions are type correct...

--}
import Data.Map hiding (map)
import Prelude hiding (lookup)
import Languate.SymbolTable
import Languate.AST
import Languate.FQN
import Languate.MaintenanceAccess.TestSemantal
import StdDef
import Data.Maybe
import State


type Context	= (FQN, Map FQN SimpleTable)



-- a value is either an expression (with substituted variables!) or an evaluated ADT
data Value	= Expr Expression
		| Primitive Expression	-- expression as Nat, Int, Float, ...
		| ADT Int [Value]
		| TupleVal [Value]
	deriving (Show)
	

-- evaluates an expression into WHNF
evalExpr	:: Context -> Expression -> Value
evalExpr _ e@(Nat _)
		=  Primitive e
evalExpr _ e@(Flt _)
		=  Primitive e
evalExpr _ e@(Chr _)
		=  Primitive e
evalExpr _ (Seq expr)
		=  todo
evalExpr _ (Tuple exprs)
		= TupleVal $ map Expr exprs
evalExpr _ (BuiltIn str)
		= todo
evalExpr ctx (Call str)
		= evalFunc ctx
evalExpr _ (Operator op)
		= todo
evalExpr _ (Cast t)
		= todo
evalExpr _ AutoCast
		= error "Not typechecked"
evalExpr _ (ExpNl _)
		= error "Not sane"


evalFunc	:: Context -> Name -> [Value] -> Expression
evalFunc ctx called args
		= let clauses	= snd $ get ctx called in
			error $ show clauses



get		:: Context -> Name -> (DocString, [Clause])
get (fqn, ctx) nm
		= fromMaybe (error $ nm++" not found in "++show fqn) $
			do	table	<- lookup fqn ctx
				find nm table







tctx		= (toFQN' "pietervdvn:Data:Prelude",t)	-- t from testSemantal
