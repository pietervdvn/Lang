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
import Languate.Interpreter.Utils
import Languate.Interpreter.BuiltIns
import Languate.Interpreter.Application
import Normalizable


fqpn		= fromJust $ toFQPN "pietervdvn:Data"
prelude		= fromJust $ toFqn' fqpn [] "Prelude"
bool		= fromJust $ toFqn' fqpn ["Data"] "Bool"


tpack	= unsafePerformIO $ typedLoad bool "../workspace/Data/src/"

ctx	= Context tpack bool []
boolT	= Normal "Bool"

r f	= runReader f ctx
r' f	= r (f >>= eval)
fetch n t
	= fromJust $ r $ searchGlobal' (Signature n $ normalize t)

idLambda	= fetch "id" $ Curry [boolT, boolT]
andLambda	= fetch "&&" $ Curry [boolT, boolT, boolT]
orLambda	= fetch "||" $ Curry [boolT, boolT, boolT]

trueDec		= fetch "True" $ Curry [boolT, Applied (Normal "Maybe") [TupleType []]]
trueCon		= fetch "True" $ Curry [boolT]
falseCon	= fetch "False" $ Curry [boolT]



t0	= r' $ apply idLambda true
t a b	= r' $ multApply [a, b] andLambda
t1 a b	= r' $ multApply [a, b] orLambda


true	= ADT 1 boolT []
false	= ADT 0 boolT []
