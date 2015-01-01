module Languate.MaintenanceAccess.TestKindConstraintBuilder where

{--

This module implements code which loads the workspace and tries to calculate the kinds of each declared type.

--}

import qualified Bnf
import Exceptions
import Languate.World
import Languate.FQN
import System.IO.Unsafe
import Languate.File2Package
import Data.Maybe
import Data.Map
import Prelude hiding (lookup)

import Languate.TypeTable
import Languate.TypeTable.BuildTypeLookupTable
--

import Languate.AST
import Languate.KindChecker.ConstructKindConstraints
import Languate.KindChecker.Solver

import Control.Monad.Reader


bnfs		= unsafePerformIO $ Bnf.load "../Parser/bnf/Languate"
packageIO	= loadPackage' bnfs (toFQN' "pietervdvn:Data:Prelude") "../workspace/Data/src/"
package		= unsafePerformIO packageIO
tlts		= buildTLTs package
kctable		= buildKindConstraintTable tlts package

-- build constraints for a simple adt: bool
t	= do	package <- packageIO
		let kctable		= buildKindConstraintTable tlts package
		print kctable
		putStrLn "\n\n---\n\n"
		runExceptionsIO' $ solve kctable

boolFQN	= toFQN' "pietervdvn:Data:Data.Bool"
colFQN	= toFQN' "pietervdvn:Data:Collection.Collection"
eqFQN	= toFQN' "pietervdvn:Data:Category.Eq"
dictFQN	= toFQN' "pietervdvn:Data:Collection.Dict"
typeFunc	= toFQN' "pietervdvn:Data:Type.Function"
maybeFQN	= toFQN' "pietervdvn:Data:Data.Maybe"
natFQN	= toFQN' "pietervdvn:Data:Num.Nat"
preludeFQN	= toFQN' "pietervdvn:Data:Prelude"


fetch fqn		= fromMaybe (error $ "Fetching "++show fqn) . lookup fqn
modul fqn	= fetch fqn $ modules package
info fqn	= Info fqn (fetch fqn tlts)
