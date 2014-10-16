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
import Languate.Order
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



bool	= fqn "Data.Bool"
functor	= fqn "Data.Functor" 
misc	= fqn "ControlFlow"
prelude	= fqn "Prelude"
bool'	= fromJust $ lookup bool package
fqn n	= toFQN' $ "pietervdvn:Data:" ++ n
fqpn	= fromJust $ toFQPN "pietervdvn:Data"

t	= do	package	<- packageIO
		return $ buildTyped fqpn priorTable package

-- default, hardcoded prioritytable
-- TODO
priorTable	:: PriorityTable
priorTable	= fromList [("+",(30, Left)),("-",(30, Left)),("*",(20, Left)),("/",(20, Left)),("%",(20, Left)), ("²", (15, Right)), (".",(10,Right)), ("|", (17, Left)), ("$", (100, Left)) ]


