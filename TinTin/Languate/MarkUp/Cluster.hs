module Languate.MarkUp.Cluster where

import StdDef

import Languate.MarkUp.MarkUp
import Languate.MarkUp.Doc
import Languate.MarkUp.HTML
import Languate.MarkUp.MD
import Data.Map (Map, findWithDefault)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

import Control.Arrow

-- A cluster is a collection of documents, which can get linked with ILink
data Cluster	= Cluster (Map Name Doc)

buildCluster	:: [Doc] -> Cluster
buildCluster docs
	= docs |> (title &&& id) & M.fromList & Cluster

data RenderSettings	= RenderSettings 
	{render		:: Doc -> String
	, renderName	:: String -> FilePath
	, preprocessor	:: MarkUp -> MarkUp}

renderClusterTo	:: RenderSettings -> FilePath -> Cluster -> IO ()
renderClusterTo	settings fp (Cluster docs)
	= mapM_ (renderFile settings fp) (M.elems docs)

renderFile	:: RenderSettings -> FilePath -> Doc -> IO ()
renderFile rs fp doc
	= do	let doc'	= preprocess (preprocessor rs) doc
		let target	= fp ++ "/" ++ _makeFPproof (renderName rs $ title doc)
		let str		= render rs doc'
		writeFile target str


html	:: RenderSettings
html	= RenderSettings renderDoc2HTML (++".html") id

md	:: RenderSettings
md	= RenderSettings renderDoc2MD (++".md") id

-- Removes illegal characters out of names
_makeFPproof	:: String -> FilePath
_makeFPproof	= filter (`notElem` "\\\"'")
