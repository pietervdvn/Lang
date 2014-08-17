module Languate.MaintenanceAccess.TestInterpreter where

{--

This module implements some testing functions.
It loads and typechecks the Data-package, and interpretes some basic statements.

--}

import Languate.TypedLoader
import Data.Maybe
import Languate.FQN
import Languate.Tools
import Data.Map as Map
import System.IO.Unsafe


bool	= fqn "Data.Bool"
functor	= fqn "Data.Functor" 
misc	= fqn "ControlFlow"
prelude	= fqn "Prelude"
fqn n	= toFQN' $ "pietervdvn:Data:" ++ n
fqpn	= fromJust $ toFQPN "pietervdvn:Data"

boolM	= fromJust $ Map.lookup bool package


t nm	= putStrLn $ head $ t' nm
t' nm	= info boolM nm
package		= unsafePerformIO packageIO
packageIO	= typedLoad prelude "../workspace/Data/src/"
