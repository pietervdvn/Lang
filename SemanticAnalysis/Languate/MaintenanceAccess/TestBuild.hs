module Languate.MaintenanceAccess.TestBuild where

{--
This module builds all the stuff!
--}

import qualified Bnf
import StdDef
import Exceptions
import Languate.MarkUp

import Data.Map as M


import Languate.File2Package
import Languate.FQN
import Languate.AST
import Languate.TAST
import Languate.Typetable

import Languate.Package
import Languate.PackageTable
import Data.Maybe

import Data.Time.Clock
import System.IO.Unsafe
import System.Directory
import Control.Monad

import Languate.ModuleTable
import Languate.Typetable.PropagateImplicitConstraints

t	= packageTabl


bnfs		= unsafePerformIO $ Bnf.load "../Parser/bnf/Languate"
path		= "../workspace/Data"
packageIO	= loadPackage' bnfs

loadedPackage	= unsafePerformIO $ packageIO path
packageTablIO	= runExceptionsIO' $ buildPackageTable loadedPackage
packageTabl	= unsafePerformIO packageTablIO


preludeFQN	= toFQN' "pietervdvn:Data:Prelude"


tt	= packageTabl & moduleTables & M.findWithDefault (error $ "Module "++show preludeFQN++" not found") preludeFQN & types



bPackT	= do	dir		<- getCurrentDirectory
		package		<- packageIO path
		runExceptionsIO' $ buildPackageTable package

bDocs	= do	dir		<- getCurrentDirectory
		packT		<- bPackT
		let cluster	= add packT (buildCluster [])
		let path'	= dir ++"/" ++ path ++ "/.gen" ++ "/html"
		exists		<- doesDirectoryExist path'
		when exists $ removeDirectoryRecursive path'
		createDirectory path'
		time	<- getCurrentTime |> utctDayTime |> realToFrac |> round
		time'	<- getCurrentTime |> show
		let hour = 2 + time `div` (60*60)
		let cssLocation	= if hour `elem` ([0..8] ++ [21..24]) then blackCSSLocation else whiteCSSLocation
		css	<- loadCSS ("../MarkUp/"++cssLocation) defaultCSSCons
		renderClusterTo (fix $ extend (setFilePath path') $ html $ defaultHeader css) cluster
