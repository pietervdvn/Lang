module Languate.MaintenanceAccess.TestSemantal where

import System.IO.Unsafe


import Languate.File2Package
import Languate.FQN
import Languate.Package2TypedPackage
import Languate.SymbolTable
import Languate.ExportBuilder
import Languate.Signature
import Languate.AST
import Languate.Precedence.Precedence
import Data.Maybe
import Prelude hiding (lookup, Left, Right)
import Data.Map as Map hiding (map)
import System.IO.Unsafe

import qualified Bnf

{--
Dev code for semantic analysis.
Tests loading, intermodule typechecking, importing, ...
--}


bnfs	= unsafePerformIO $ Bnf.load "../Parser/bnf/Languate"
packageIO	= loadPackage' bnfs prelude "../workspace/Data/src/"
package	= unsafePerformIO packageIO
priorTable	= buildPrecTable $ elems package


bool	= fqn "Data.Bool"
functor	= fqn "Data.Functor"
misc	= fqn "ControlFlow"
prelude	= fqn "Prelude"
bool'	= fromJust $ lookup bool package
fqn n	= toFQN' $ "pietervdvn:Data:" ++ n
fqpn	= fromJust $ toFQPN "pietervdvn:Data"

t	= do	package	<- packageIO
		let tpack = buildTyped fqpn priorTable package
		return tpack
