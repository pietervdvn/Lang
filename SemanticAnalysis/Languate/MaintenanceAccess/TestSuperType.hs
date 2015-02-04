module Languate.MaintenanceAccess.TestSuperType where

{--
This module implements tests for the super type relationship
--}
import StdDef
import qualified Bnf
import Exceptions


import Languate.File2Package
import Languate.FQN
import Languate.AST
import Languate.World

import Languate.TableOverview
import Languate.MD.TableOverview2MD

import Languate.TypeTable.Bind

import Languate.TypeTable
import Languate.TAST

import System.IO.Unsafe
import System.Directory

import StateT

import Data.Map (empty, fromList)



bnfs		= unsafePerformIO $ Bnf.load "../Parser/bnf/Languate"
path		= "../workspace/Data"
packageIO	= loadPackage' bnfs (toFQN' "pietervdvn:Data:Prelude")

toIO	= do	world	<- packageIO $ path++"/src/"
		runExceptionsIO' $ buildAllTables world

to	= unsafePerformIO toIO
tt	= typeTable to

test t0 t1 reqs = bind tt reqs t0 t1
test' t	= superTypesOf tt t

-- basic test
t0	= test natT (RFree "a") (fromList [("a",[eqC]),("b",[eqC])])
-- fails
t1	= test anyType (RFree "a") (fromList [("a", [eqC])])
-- no binding (but does not fail)
t2	= test natT anyType empty
-- fails
t3	= test anyType natT empty
-- binds {a --> natT, b --> intT} by recursive binding
t4	= test (RApplied (RApplied curryT natT) intT)  (RCurry (RFree "a") $ RFree "b") empty
-- binds {a --> List Nat, b --> Nat}. The "b" is bound via the type requirements
t5	= test (RApplied list natT) (RFree "a")
		(fromList [("a", [RApplied list $ RFree "b"])])

t6	= test (RCurry natT intT) (RCurry (RFree "a") (RFree "b")) empty




natT	= RNormal (fqn $ "Num.Nat") "Nat"
intT	= RNormal (fqn $ "Num.Nat") "Int"

prod	= RNormal (fqn $ cat "Monoid") "Product"
sumM	= RNormal (fqn $ cat "Monoid") "Sum"
monoid	= RNormal (fqn $ cat "Monoid") "Monoid"

list	= RNormal (fqn $ col "List") "List"
coll	= RNormal (fqn $ col "Collection") "Collection"
eqC	= RNormal (fqn $ cat "Eq") "Eq"
mapC	= RNormal (fqn $ cat "Mappable") "Mappable"

curryT	= RNormal (fqn $ "Category.Function") "Curry"
assocT	= RNormal (fqn $ "Category.Function") "Associative"

fqn	= toFQN' . (++) "pietervdvn:Data:"
cat	= (++) "Category."
col	= (++) "Collection."
