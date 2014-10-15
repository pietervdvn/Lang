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


tpack	= unsafePerformIO $ typedLoad prelude "../workspace/Data/src/"

ctx	= Context tpack prelude []
boolT	= Normal "Bool"
natT	= Normal "Nat"

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
conv		= fetch "convert" $ Curry [boolT, Normal "Nat"]
ifF		= fetch "if" $ Curry [boolT, Free "a", Free "a", Free "a"]
	
t0	= r' $ apply idLambda true
t a b	= r' $ multApply [a, b] andLambda
t1 a b	= r' $ multApply [a, b] orLambda
t2 a	= r' $ multApply [a] conv
t3 b x y
	= r' $ multApply [b, x, y] ifF

true	= ADT 1 boolT []
false	= ADT 0 boolT []


forty		= ADT 40 boolT []
fortyOne	= ADT 41 boolT []
fortyTwo	= ADT 42 boolT []


