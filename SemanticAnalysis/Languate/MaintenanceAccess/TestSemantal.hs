module Languate.MaintenanceAccess.TestSemantal where

import System.IO.Unsafe
import Languate.File2Package
import Languate.FQN
import Data.Map
import Prelude hiding (lookup)
import Languate.SymbolTable
import Data.Maybe
import Languate.AST
import Languate.BuiltIns
import Languate.TypeChecker
{--
Dev code for semantic analysis.
--}

package	= unsafePerformIO $ loadPackage' (toFQN' "pietervdvn:Data:Prelude") "../workspace/Data/src/"

bool	= toFQN' "pietervdvn:Data:Data.Bool"
bool'	= fromJust $ lookup bool package

t	= buildWithImports package


tt	= TT Empt $ fromList [("+",[ [nat,nat] --> nat])]
tctx	= Context tt $ fromList [("+",3),("-",3),("*",2),("/",2),("%",2)]

-- type check test
tc 	= typeCheck tctx $ Seq [ Nat 1, Operator "+", Nat 2, Operator "*", Nat 2]
