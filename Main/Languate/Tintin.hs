module Languate.Tintin where

{--
This module implements
--}

import StdDef
import Languate.MarkUp as Mu
import Languate.Pipeline

import System.Directory
import Data.Time.Clock

import Languate.Package



bDocs	:: FilePath -> Context -> IO ()
bDocs path (Context package tablesOverv)
	= do	dir	<- getCurrentDirectory
		let cluster	= buildCluster []
		let cluster'	= add tablesOverv cluster
		addFooter'	<- addFooter
		let path'	= dir ++"/" ++ path ++ "/.gen" ++ "/html"
		time	<- getCurrentTime |> utctDayTime |> realToFrac |> round
		let hour = 2 + time `div` (60*60)
		let css	= if hour `elem` ([0..8] ++ [21..24]) then blackCSS else defaultCSS
		removeDirectoryRecursive path'
		renderClusterTo (fix $ extend (setFilePath path' . addFooter' . addHeader) $ html $ defaultHeader css) cluster'
		putStrLn "Documentation rendered!"


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
