module Languate.MaintenanceAccess.TestSemantal where

import System.IO.Unsafe


import Languate.File2Package
import Languate.FQN
import Languate.Package2TypedPackage
import Languate.SymbolTable
import Languate.ExportBuilder
import Languate.Signature
import Languate.AST
import Data.Maybe
import Prelude hiding (lookup)
import Data.Map as Map hiding (map)


{--
Dev code for semantic analysis.
Tests loading, intermodule typechecking, importing, ...
--}


packageIO	= loadPackage' prelude "../workspace/Data/src/"
package	= unsafePerformIO $ packageIO


bool	= fqn "Data.Bool"
functor	= fqn "Data.Functor" 
misc	= fqn "ControlFlow"
prelude	= fqn "Prelude"
bool'	= fromJust $ lookup bool package
fqn n	= toFQN' $ "pietervdvn:Data:" ++ n
fqpn	= fromJust $ toFQPN "pietervdvn:Data"

t	= do	package	<- packageIO
		return $ buildTyped fqpn package
