module Languate.MaintenanceAccess.TestBuildTypeTable where


import Languate.File2Package
import Languate.FQN
import Languate.AST
import Data.Maybe
import Languate.World
import System.IO.Unsafe
import qualified Bnf
import Data.Map
import Prelude hiding (lookup)

import Languate.TypeTable.BuildTypeLookupTable
import Languate.TypeTable.BuildTypeTable
import Languate.TypeTable

import Exceptions
import Languate.Checks
{--
Dev code for semantic analysis.
--}


bnfs		= unsafePerformIO $ Bnf.load "../Parser/bnf/Languate"
packageIO	= loadPackage' bnfs (toFQN' "pietervdvn:Data:Prelude") "../workspace/Data/src/"
package		= unsafePerformIO packageIO

tlts	= buildTLTs package

preludeFQN	= toFQN' "pietervdvn:Data:Prelude"
boolFQN	= toFQN' "pietervdvn:Data:Data.Bool"


t	= runExceptionsIO' $ buildTypeTable package

pre	= fromJust $ lookup preludeFQN tlts
bl	= fromJust $ lookup boolFQN tlts
