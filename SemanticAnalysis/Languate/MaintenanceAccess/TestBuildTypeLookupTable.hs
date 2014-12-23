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
import Languate.TypeTable

{--
Dev code for semantic analysis.
--}


bnfs		= unsafePerformIO $ Bnf.load "../Parser/bnf/Languate"
packageIO	= loadPackage' bnfs (toFQN' "pietervdvn:Data:Prelude") "../workspace/Data/src/"
package		= unsafePerformIO packageIO

t	= buildTLTs package

preludeFQN	= toFQN' "pietervdvn:Data:Prelude"

{-
pre	= fromJust $ lookup preludeFQN t
preludeMod	= fromJust $ lookup preludeFQN $ modules package



monoidFQN	= toFQN' "pietervdvn:Data:Category.Monoid"
monoidMod	= fromJust $ lookup monoidFQN $ modules package

--testRes	nms nm
--	= resolveType (nms, nm) $ fromJust $ lookup preludeFQN t

--}
