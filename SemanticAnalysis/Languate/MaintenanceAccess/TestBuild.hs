module Languate.MaintenanceAccess.TestBuild where

{--
This module builds all the stuff!
--}

import qualified Bnf
import Exceptions


import Languate.File2Package
import Languate.FQN
import Languate.AST
import Languate.Package
import Languate.MarkUp

import System.IO.Unsafe
import System.Directory


import Languate.Precedence.PrecedenceTable
import Languate.Precedence.BuildPrecedenceTable
import Languate.TypeTable
import Languate.TypeTable.BuildTypeLookupTable
import Languate.TypeTable.BuildTypeTable
import Languate.TypeTable.TypeTable2mu

import Data.Map
import StdDef

bnfs		= unsafePerformIO $ Bnf.load "../Parser/bnf/Languate"
path		= "../workspace/Data"
packageIO	= loadPackage' bnfs (toFQN' "pietervdvn:Data:Prelude")

t	= do	w	<- packageIO path
		dir	<- getCurrentDirectory
		let cluster	= buildCluster []
		precT	<- runExceptionsIO' $ buildPrecTable' w
		tt	<- runExceptionsIO' $ buildTypeTable w
		let cluster'	= add tt $ add precT cluster
		renderClusterTo html (dir ++"/" ++ path ++ "/.gen" ++ "/html2") cluster'
		renderClusterTo md (dir ++"/" ++ path ++ "/.gen" ++ "/md2") cluster'
