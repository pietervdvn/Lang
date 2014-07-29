module Languate.ModuleTypeChecker where

import Languate.Signature
import StdDef
import Languate.AST
import Languate.SymbolTable
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


typeCheckPackage	:: Map FQN SimpleTable -> Map FQN (SymbolTable (DocString, [TClause]))
typeCheckPackage	=  Map.map $ typeCheckModule priorTable



typeCheckModule		:: PriorityTable -> SimpleTable ->  SymbolTable (DocString, [TClause])
typeCheckModule prior st	
			=  let tt	= buildTypeTable st in
				mapWithType (\t -> second $ checkFunction tt prior t) st


checkFunction		:: TypeTable -> PriorityTable -> Type -> [Clause] -> [TClause]
checkFunction tt prior t
			=  map $ checkClause tt prior t


-- default, hardcoded prioritytable
-- TODO
priorTable	:: PriorityTable
priorTable	= fromList [("+",(30, Left)),("-",(30, Left)),("*",(20, Left)),("/",(20, Left)),("%",(20, Left)), ("²", (15, Right)), (".",(10,Right)), ("|", (17, Left)), ("$", (100, Left)) ]

