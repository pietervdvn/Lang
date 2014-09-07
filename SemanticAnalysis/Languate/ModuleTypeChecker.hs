module Languate.ModuleTypeChecker where

import Languate.Signature
import StdDef
import Languate.AST
import Languate.SymbolTable
import Languate.TypeTable
import Languate.TypeChecker
import Languate.Order
import Prelude hiding (Right, Left)
import Data.Map (Map, fromList)
import qualified Data.Map as Map

import Languate.FQN
import Languate.FunctionTypeChecker
import Control.Arrow

{--

This module typechecks an entire package. It assumes that all other packages are typechecked.

Note the usage of the word **package**, not **module**. Cyclical imports are allowed within the same package.

For now, all types should be given explicitly; the typechecker merely checks if the types are correct.

In later versions, types will be inferred, allowing implicit typing.
--}


typeCheckModule		:: PriorityTable -> SymbolTable (Statement,[Clause]) -> (SymbolTable Statement, SymbolTable [TClause])
typeCheckModule prior st	
			=  let tt	= buildTypeTable st in
				-- some clauses have special builtins which can not be typechecked and should be regenerated after typechecking. It are these statements:
			   let refusedStms	= filterTable hasSpecialBuiltin st in
				mapWithType (checkFunction tt prior) $ filterTable (not . hasSpecialBuiltin) st


checkFunction		:: TypeTable -> PriorityTable -> Type -> [Clause] -> [TClause]
checkFunction tt prior t
			=  map $ checkClause tt prior t

-- true if the function contains special builtings, such as 'asADT'. These functions should NOT be typechecked, as a typed version is directly provided/injected into the package
hasSpecialBuiltin	:: [Clause] -> Bool
hasSpecialBuiltin	= any _hasSpecBuilt

_hasSpecBuilt		:: Clause -> Bool
_hasSpecBuilt (Clause pts expr)
			= any ptBuiltIn pts || exprBuiltIn expr

exprBuiltIn		:: Expression -> Bool
exprBuiltIn (Seq es)	=  any exprBuiltIn es
exprBuiltIn (Tuple es)	=  any exprBuiltIn es
exprBuiltIn (BuiltIn name)
			= name `elem` ["asADT","fromADT","asTuple","fromTuple"]
exprBuiltIn (Call c)	= head c == '#' && error ("Still some builtin "++ show c ++"with a sharp prefix! Call pietervdvn")
exprBuiltIn _		= False



ptBuiltIn		:: Pattern -> Bool
ptBuiltIn (Let _ _)	=  todos "ModuleTypeChecker: check patterns on special builtins: let expressions"
ptBuiltIn (Deconstruct _ pts)
			= any ptBuiltIn pts
ptBuiltIn (Multi pts)	= any ptBuiltIn pts
ptBuiltIn (Eval e)	= exprBuiltIn e
ptBuiltIn _		= False
