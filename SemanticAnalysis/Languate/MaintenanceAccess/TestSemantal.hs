module Languate.MaintenanceAccess.TestSemantal where

import System.IO.Unsafe


import Languate.File2Package
import Languate.FQN
import Languate.Package2TypedPackage
import Languate.SymbolTable
import Languate.ExportBuilder
import Data.Maybe
import Prelude hiding (lookup)
import Data.Map as Map hiding (map)


{--
Dev code for semantic analysis.
Tests loading, intermodule typechecking, importing, ...
--}

package	= unsafePerformIO $ loadPackage' (toFQN' "pietervdvn:Data:Prelude") "../workspace/Data/src/"

localBuild	= mapWithKey buildLocal package


bool	= fqn "Data.Bool"
functor	= fqn "Data.Functor" 
misc	= fqn "ControlFlow"
bool'	= fromJust $ lookup bool package
fqn n	= toFQN' $ "pietervdvn:Data:" ++ n
fqpn	= fromJust $ toFQPN "pietervdvn:Data"

testBuildexports	= buildExports fqpn package $ localDeclared localBuild
