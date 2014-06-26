module Languate.MaintenanceAccess.TestSemantal where

import System.IO.Unsafe
import Languate.File2Package
import Languate.FQN
import Data.Map
import Prelude hiding (lookup)
import Languate.SymbolTable
import Data.Maybe
import Languate.AST
{--
Dev code for semantic analysis.
--}

package	= unsafePerformIO $ loadPackage' (toFQN' "pietervdvn:Data:Prelude") "../workspace/Data/src/"

bool	= toFQN' "pietervdvn:Data:Data.Bool"
bool'	= fromJust $ lookup bool package

t	= buildWithImports package
