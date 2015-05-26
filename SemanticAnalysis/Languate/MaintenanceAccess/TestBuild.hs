module Languate.MaintenanceAccess.TestBuild where

{--
This module builds all the stuff!
--}

import qualified Bnf
import StdDef
import Exceptions


import Languate.File2Package
import Languate.FQN
import Languate.AST
import Languate.MarkUp as Mu

import System.IO.Unsafe
import System.Directory


import Languate.TableOverview
import Languate.BuildTableOverview

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
import Data.Time.Clock
import Data.Time.LocalTime

bnfs		= unsafePerformIO $ Bnf.load "../Parser/bnf/Languate"
path		= "../workspace/Data"
packageIO	= loadPackage' bnfs (toFQN' "pietervdvn:Data:Prelude")

loadedPackage	= unsafePerformIO $ packageIO path
tablesOvervIO	= runExceptionsIO' $ buildAllTables loadedPackage
tablesOverv	= unsafePerformIO tablesOvervIO
prelude		= toFQN' "pietervdvn:Data:Prelude"
bool		= toFQN' "pietervdvn:Data:Data.Bool"


bDocs	= do	dir	<- getCurrentDirectory
		let cluster	= buildCluster []
		let cluster'	= add tablesOverv cluster
		addFooter'	<- addFooter
		let path'	= dir ++"/" ++ path ++ "/.gen" ++ "/html"
		time	<- getCurrentTime |> utctDayTime |> realToFrac |> round
		let hour = 2 + time `div` (60*60)
		let css	= if hour `elem` ([0..8] ++ [21..24]) then blackCSS else defaultCSS
		removeDirectoryRecursive path'
		renderClusterTo (fix $ extend (setFilePath path' . addFooter' . addHeader) $ html $ defaultHeader css) cluster'


links		:: MarkUp -> Maybe MarkUp
links (InLink mu nm)
		= Just $ InLink mu (nm ++ ".html")
links _		= Nothing

addFooter	:: IO (RenderSettings -> RenderSettings)
addFooter	= do	let back = NonImp $ InLink (Base "Back to all pages") "All pages"
			time	<- getCurrentTime
			print time
			return $ addPreprocessor' (\mu -> Mu.Seq [back, mu,
				NonImp $ parags [Base $ "This doc was automatically generated on "++show time, Base "Do not edit it, as changes will be erased the next generation.", back]])

addHeader	:: RenderSettings -> RenderSettings
addHeader	= addPreprocessor (\doc -> preprocess (\mu -> titling (title doc) $ Mu.Seq [notImportant $ description doc, contents doc ]) doc)
