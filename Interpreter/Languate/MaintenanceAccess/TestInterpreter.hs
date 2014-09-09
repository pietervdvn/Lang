module Languate.MaintenanceAccess.TestInterpreter where

{--

This module implements some testing functions.
It loads and typechecks the Data-package, and interpretes some basic statements.

--}

import Languate.TypedLoader
import Data.Maybe
import Languate.FQN
import Languate.Interpreter.Tools
import Languate.Interpreter.Utils
import Data.Map as Map
import System.IO.Unsafe
import Languate.Interpreter
import Languate.InterpreterDef
import Control.Monad.Reader
import Languate.AST
import Languate.TAST
import Normalizable


fqn n	= toFQN' $ "pietervdvn:Data:" ++ n
fqpn	= fromJust $ toFQPN "pietervdvn:Data"

bool	= fqn "Data.Bool"
functor	= fqn "Data.Functor" 
misc	= fqn "ControlFlow"
prelude	= fqn "Prelude"

fetch fqn
	= fromJust $ Map.lookup fqn package
boolM	= fetch bool
preludeM	= fetch prelude



t0	= runReader (expand $ VCall bool [normalize $ Curry [Normal "Bool",Normal "Bool",Normal "Bool"]] "&&") ctx

deconstr	= VCall bool [normalize $ Curry [Normal "Bool",Applied (Normal "Maybe") [(TupleType [])]]] "True"

constr str
	= VCall bool [Normal "Bool"] str

t' x	= runReader (expand x) ctx

t	= runReader (matchPattern bool (ADT (Normal "Bool") 0 "FalseADT") (TDeconstruct "True" [])) ctx


ctx	= Context package

package		= unsafePerformIO packageIO
packageIO	= typedLoad prelude "../workspace/Data/src/"


inf str	= putStrLn $ info boolM str
