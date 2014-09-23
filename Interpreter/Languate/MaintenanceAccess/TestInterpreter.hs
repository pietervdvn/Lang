module Languate.MaintenanceAccess.TestInterpreter where

{--
--}


import StdDef
import System.IO.Unsafe
import Data.Maybe
import Control.Monad.Reader

import Languate.TypedLoader
import Languate.FQN
import Languate.InterpreterDef
import Languate.SymbolTable
import Languate.Signature
import Languate.Interpreter
import Languate.AST
import Languate.TAST


fqpn		= fromJust $ toFQPN "pietervdvn:Data"
prelude		= fromJust $ toFqn' fqpn [] "Prelude"
bool		= fromJust $ toFqn' fqpn ["Data"] "Bool"


tpack	= unsafePerformIO $ typedLoad bool "../workspace/Data/src/"

ctx	= Context tpack bool (Empty)

t	= runReader (evalExpr (TCall [boolT] "t") boolT) ctx

t'	= runReader (evalExpr (TCall [boolT] "False") boolT) ctx

boolT	= Normal "Bool"
