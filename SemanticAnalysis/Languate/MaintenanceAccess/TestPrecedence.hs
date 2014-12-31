module Languate.MaintenanceAccess.TestPrecedence where


import Languate.File2Package
import Languate.FQN
import Languate.AST
import Languate.World
import Data.Maybe
import Data.Map (toList)
import qualified Data.Map as Map
import Prelude hiding (lookup)

import Languate.Precedence.PrecedenceTable
import Languate.Precedence.Expr2PrefExpr

import System.IO.Unsafe

import qualified Bnf
import Exceptions

{--
Dev code for semantic analysis.
Tests loading, intermodule typechecking, importing, ...
--}


bnfs		= unsafePerformIO $ Bnf.load "../Parser/bnf/Languate"
packageIO	= loadPackage' bnfs (toFQN' "pietervdvn:Data:Prelude") "../workspace/Data/src/"
package		= unsafePerformIO packageIO


bool	= fqn "Data.Bool"
functor	= fqn "Data.Functor"
misc	= fqn "ControlFlow"
prelude	= fqn "Prelude"
dict	= fqn "Collections.Dict"

fqn n	= toFQN' $ "pietervdvn:Data:" ++ n
fqpn	= fromJust $ toFQPN "pietervdvn:Data"

t	= do	package	<- packageIO
		let mods	= map snd $ toList $ modules package
		precTable	<- runExceptionsIO' $ buildPrecTable package
		print precTable
		print $ expr2prefExpr precTable expr
		print $ expr2prefExpr precTable expr0


expr	= Seq [Operator "!", Operator "!", Nat 5, Operator "+", Nat 4, Operator "*", Nat 3, Operator "%", Nat 5, Operator "+", Nat 5, Operator "²", Operator "³"]
expr0	= Seq [Nat 1, Operator "-->", Nat 2, Operator "<--", Nat 3]
