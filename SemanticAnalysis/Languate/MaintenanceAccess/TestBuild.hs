module Languate.MaintenanceAccess.TestBuild where

{--
This module builds all the stuff!
--}

import qualified Bnf
import Exceptions


import Languate.File2Package
import Languate.FQN
import Languate.AST
import Languate.World

import Languate.BuildTables

import System.IO.Unsafe


bnfs		= unsafePerformIO $ Bnf.load "../Parser/bnf/Languate"
path		= "../workspace/Data"
packageIO	= loadPackage' bnfs (toFQN' "pietervdvn:Data:Prelude")

t	= do	world	<- packageIO $ path++"/src/"
		to	<- runExceptionsIO' $ buildAllTables world
		writeTables to path
		putStrLn "Written MDs!"
