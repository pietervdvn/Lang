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
import System.Directory


bnfs		= unsafePerformIO $ Bnf.load "../Parser/bnf/Languate"
path		= "../workspace/Data"
packageIO	= loadPackage' bnfs (toFQN' "pietervdvn:Data:Prelude")

tables	= do	world	<- packageIO $ path++"/src/"
		runExceptionsIO' $ buildAllTables world


t	= do	to	<- tables
		writeTables to path
		wd	<- getCurrentDirectory
		let wd'	= wd ++ "/" ++ path ++ "/.gen/html"
		putStrLn $ "Written MDs! See file:///"++wd'++"/Index.html"
