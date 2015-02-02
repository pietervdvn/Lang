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

import Languate.TypeTable.IsSuperType
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

test t0 t1 reqs = runstateT (b' t0 t1) (Context reqs tt noBinding) |> snd

t	= test (RApplied list [RApplied coll [anyType]]) (RFree "a") (fromList [("a",[eqC]),("b",[eqC])])

intT	= RNormal (fqn $ "Num.Nat") "Nat"

prod	= RNormal (fqn $ cat "Monoid") "Product"
sumM	= RNormal (fqn $ cat "Monoid") "Sum"
monoid	= RNormal (fqn $ cat "Monoid") "Monoid"

list	= RNormal (fqn $ col "List") "List"
coll	= RNormal (fqn $ col "Collection") "Collection"
eqC	= RNormal (fqn $ cat "Eq") "Eq"
mapC	= RNormal (fqn $ cat "Mappable") "Mappable"

fqn	= toFQN' . (++) "pietervdvn:Data:"
cat	= (++) "Category."
col	= (++) "Collection."
