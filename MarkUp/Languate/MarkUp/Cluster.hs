module Languate.MarkUp.Cluster where

import StdDef
import HumanUtils

import Languate.MarkUp.MarkUp
import Languate.MarkUp.Doc
import Languate.MarkUp.HTML
import Languate.MarkUp.MD
import Data.Map (Map, findWithDefault)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

import Control.Arrow
import Control.Monad

import Debug.Trace

-- A cluster is a collection of documents, which can get linked with ILink
data Cluster	= Cluster (Map Name Doc)

buildCluster	:: [Doc] -> Cluster
buildCluster docs
	= docs |> (title &&& id) & M.fromList & Cluster

data RenderSettings	= RenderSettings 
	{render		:: Doc -> String
	, renderName	:: String -> FilePath
	, preprocessor	:: MarkUp -> MarkUp}

html	:: RenderSettings
html	= RenderSettings renderDoc2HTML (++".html") id

md	:: RenderSettings
md	= RenderSettings renderDoc2MD (++".md") id

renderClusterTo	:: RenderSettings -> FilePath -> Cluster -> IO ()
renderClusterTo	settings fp cluster@(Cluster docs)
	= do	mapM_ (renderFile cluster settings fp) (M.elems docs)
		putStrLn $ "Written document cluster to "++fp++" containing "++ commas (M.keys docs)


renderFile	:: Cluster -> RenderSettings -> FilePath -> Doc -> IO ()
renderFile (Cluster docs) rs fp doc= do
	let doc'	= preprocess (rewrite (_renderInLink rs fp) . preprocessor rs) doc
	let inLinks	= search searchInLinks $ contents doc
	let deadLinks	= inLinks & filter (`notElem` M.keys docs)
	unless (null deadLinks) $ putStrLn $ "Warning: the document "++show (title doc)++" contains some dead internal links: "++unwords (deadLinks |> show)
	let target	= _targetName rs fp $ title doc
	let str		= render rs doc'
	writeFile target str


searchInLinks	:: MarkUp -> Maybe Name
searchInLinks (InLink _ nm)
	= Just nm
searchInLinks _	= Nothing

_renderInLink	:: RenderSettings -> FilePath -> MarkUp -> Maybe MarkUp
_renderInLink rs fp (InLink mu docName)
	= Just $ Link mu $ _targetName rs fp docName
_renderInLink _ _ _	= Nothing

_targetName	:: RenderSettings -> FilePath -> Name -> FilePath
_targetName rs fp nm
	= fp ++ "/" ++ _makeFPproof (renderName rs nm)


-- Removes illegal characters out of names
_makeFPproof	:: String -> FilePath
_makeFPproof	= filter (`notElem` "\\\"'")
