module Languate.MaintenanceAccess.TestBuild where

{--
This module builds all the stuff!
--}

import qualified Bnf
import StdDef
import Exceptions
import Languate.MarkUp


import Languate.File2Package
import Languate.FQN
import Languate.AST
import Languate.TAST
import Languate.Typetable

import Languate.Package
import Languate.PackageTable


import System.IO.Unsafe
import System.Directory

t	= packageTabl


bnfs		= unsafePerformIO $ Bnf.load "../Parser/bnf/Languate"
path		= "../workspace/Data"
packageIO	= loadPackage' bnfs

loadedPackage	= unsafePerformIO $ packageIO path
packageTablIO	= runExceptionsIO' $ buildPackageTable loadedPackage
packageTabl	= unsafePerformIO packageTablIO
prelude		= toFQN' "pietervdvn:Data:Prelude"
bool		= toFQN' "pietervdvn:Data:Data.Bool"

bDocs	= do	dir		<- getCurrentDirectory
		let cluster	= buildCluster []
		package		<- packageIO path
		packT		<- runExceptionsIO' $ buildPackageTable package
		let cluster'	= add packT cluster
		let path'	= dir ++"/" ++ path ++ "/.gen" ++ "/html"
		removeDirectoryRecursive path'
		renderClusterTo (fix $ extend (setFilePath path') $ html $ defaultHeader defaultCSS) cluster'
