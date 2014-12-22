module Languate.MaintenanceAccess.TestKindChecker where

{--

This module implements code which loads the workspace and tries to calculate the kinds of each declared type.

--}

import qualified Bnf
import Languate.World
import Languate.FQN
import System.IO.Unsafe
import Languate.File2Package
import Data.Maybe
import Data.Map
import Prelude hiding (lookup)
--

import Languate.KindChecker.ConstructKindConstraints


bnfs		= unsafePerformIO $ Bnf.load "../Parser/bnf/Languate"
packageIO	= loadPackage' bnfs (toFQN' "pietervdvn:Data:Prelude") "../workspace/Data/src/"
package		= unsafePerformIO packageIO


-- build constraints for a simple adt: bool
t1		=

boolFQN	= toFQN' "pietervdvn:Data:Data.Bool"
boolMod	= fromJust $ lookup boolFQN $ modules package
