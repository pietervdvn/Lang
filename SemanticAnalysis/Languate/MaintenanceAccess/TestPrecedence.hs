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
import Data.Map (toList, )

import Languate.Precedence.PrecedenceTable

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
fqn n	= toFQN' $ "pietervdvn:Data:" ++ n
fqpn	= fromJust $ toFQPN "pietervdvn:Data"

t	= do	package	<- packageIO
		let mods	= map snd $ toList package
		return $ buildPrecTable mods
