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


import Languate.TableOverview

import Languate.Package
import Languate.TypeTable
import Languate.TypeTable.BuildKnownTypes
import Languate.TypeTable.BuildTypeLookupTable
import Languate.TypeTable.KindChecker.BuildKindTable
import Languate.TypeTable.BuildRequirementTable
import Languate.TypeTable.BuildDocstringTable
import Languate.TypeTable.BuildFreeNameTable
import Languate.TypeTable.BuildSuperTT.BuildSuperTypeTable
import Languate.TypeTable.BuildSuperTT.BuildSuperTypeTableFull
import Languate.TypeTable.BuildSuperTT.FixImplicitRequirements
import Languate.TypeTable.BuildSuperTT.ExpandFSTT

import Languate.TypeTable.Checks.CheckPackage
import Languate.TypeTable.Checks.CheckReqTable

import Languate.CheckUtils

import Data.Set as S

import Data.Map
import StdDef

bnfs		= unsafePerformIO $ Bnf.load "../Parser/bnf/Languate"
path		= "../workspace/Data"
packageIO	= loadPackage' bnfs (toFQN' "pietervdvn:Data:Prelude")

loadedPackage	= unsafePerformIO $ packageIO path
tablesOvervIO	= runExceptionsIO' $ buildAllTables loadedPackage
tablesOverv	= unsafePerformIO tablesOvervIO
prelude		= toFQN' "pietervdvn:Data:Prelude"



bDocs	= do	dir	<- getCurrentDirectory
		let cluster	= buildCluster []
		let cluster'	= add tablesOverv cluster
		renderClusterTo ((extend addFooter html) $ dir ++"/" ++ path ++ "/.gen" ++ "/html")
				cluster'


addFooter	:: RenderSettings -> RenderSettings
addFooter	= let back = NonImp $ InLink (Base "Back to all pages") "All pages" in
			addPreprocessor' (\mu -> parags [back, mu, back])
