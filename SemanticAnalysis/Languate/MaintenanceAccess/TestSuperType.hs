module Languate.MaintenanceAccess.TestSuperType where

{--
This module implements tests for the super type relationship
--}

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

import Data.Map (empty)



bnfs		= unsafePerformIO $ Bnf.load "../Parser/bnf/Languate"
path		= "../workspace/Data"
packageIO	= loadPackage' bnfs (toFQN' "pietervdvn:Data:Prelude")

toIO	= do	world	<- packageIO $ path++"/src/"
		runExceptionsIO' $ buildAllTables world

to	= unsafePerformIO toIO
tt	= typeTable to

t	= isSupertypeOf tt empty

prod	= RNormal (fqn $ cat "Monoid") "Product"
sumM	= RNormal (fqn $ cat "Monoid") "Sum"
monoid	= RNormal (fqn $ cat "Monoid") "Monoid"

coll	= RNormal (fqn $ col "Collection") "Collection"
mapC	= RNormal (fqn $ cat "Mappable") "Mappable"

fqn	= toFQN' . (++) "pietervdvn:Data:"
cat	= (++) "Category."
col	= (++) "Collection."
