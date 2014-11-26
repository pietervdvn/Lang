module Languate.MaintenanceAccess.TestBuildTypeLookupTable where


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
import Languate.TypeTable.TypeTable

{--
Dev code for semantic analysis.
--}


bnfs		= unsafePerformIO $ Bnf.load "../Parser/bnf/Languate"
packageIO	= loadPackage' bnfs (toFQN' "pietervdvn:Data:Prelude") "../workspace/Data/src/"
package		= unsafePerformIO packageIO


t	= buildTypeLookupTable package



monoidFQN	= toFQN' "pietervdvn:Data:Category.Monoid"
monoidMod	= fromJust $ lookup monoidFQN $ modules package
preludeFQN	= toFQN' "pietervdvn:Data:Prelude"
preludeMod	= fromJust $ lookup preludeFQN $ modules package


--testRes	nms nm
--	= resolveType (nms, nm) $ fromJust $ lookup preludeFQN t
