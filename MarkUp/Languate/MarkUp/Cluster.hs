module Languate.MarkUp.Cluster where

import StdDef
import HumanUtils

import Languate.MarkUp.MarkUp
import Languate.MarkUp.Doc
import Languate.MarkUp.Classes
import Languate.MarkUp.HTML
import Languate.MarkUp.MD
import Data.Map (Map, findWithDefault)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Maybe

import Control.Arrow
import Control.Monad

import Debug.Trace

-- A cluster is a collection of documents, which can get linked with ILink
data Cluster	= Cluster (Map Name Doc)

buildCluster	:: [Doc] -> Cluster
buildCluster docs
	= docs |> (title &&& id) & M.fromList & Cluster

add	:: (Documentable documentable) => documentable -> Cluster -> Cluster
add docable (Cluster docs)
	= let 	(doc, docs')	= toDocument docable 
		newDocs	= (doc:docs') |> (title &&& id) in
		Cluster $ M.union docs $ M.fromList newDocs

data RenderSettings	= RenderSettings 
	{render		:: Doc -> String
	, renderName	:: String -> FilePath
	, embedder	:: Doc -> MarkUp
	, preprocessor	:: MarkUp -> MarkUp
	, overviewPage	:: Maybe ([Doc] -> Doc)}

html	:: RenderSettings
html	= RenderSettings renderDoc2HTML (++".html") fancyEmbedder id (Just defaultOverviewPage)

md	:: RenderSettings
md	= RenderSettings renderDoc2MD (++".md") fancyEmbedder id (Just defaultOverviewPage)

fancyEmbedder doc
	= titling (title doc) $ Seq [notImportant $ description doc, contents doc]

defaultOverviewPage	:: [Doc] -> Doc
defaultOverviewPage docs
	= let	titl	= "All pages"
		descr	= "Overview of all pages within the cluster"
		genEntry doc
			= [inlink $ title doc, Base $ description doc]
		tbl	= table ["Title", "Description"] $ docs |> genEntry in
		Doc titl descr M.empty $ titling titl tbl

renderClusterTo	:: RenderSettings -> FilePath -> Cluster -> IO ()
renderClusterTo	settings fp cluster@(Cluster docs)
	= do	mapM_ (renderFile cluster settings fp) (M.elems docs)
		case overviewPage settings of
			Just overview	-> renderFile cluster settings fp $ overview $ M.elems docs
			Nothing		-> return ()
		putStrLn $ "Written document cluster to "++fp++" containing "++ commas (M.keys docs)


renderFile	:: Cluster -> RenderSettings -> FilePath -> Doc -> IO ()
renderFile cluster@(Cluster docs) rs fp doc= do
	let doc'	= preprocess (rewrite (_renderEmbed cluster rs) . rewrite (_renderInLink rs fp) . preprocessor rs) doc
	let inLinks	= search searchRefs $ contents doc
	let deadLinks	= inLinks & filter (`notElem` M.keys docs)
	unless (null deadLinks) $ putStrLn $ "Warning: the document "++show (title doc)++" contains some dead internal links, namely "++commas deadLinks
	let target	= _targetName rs fp $ title doc
	let str		= render rs doc'
	writeFile target str


-- Returns all markups with references to different docs for error msgs
searchRefs	:: MarkUp -> Maybe Name
searchRefs (InLink _ nm)
	= Just nm
searchRefs (Embed nm)
	= Just nm
searchRefs _	= Nothing



_renderInLink	:: RenderSettings -> FilePath -> MarkUp -> Maybe MarkUp
_renderInLink rs fp (InLink mu docName)
	= Just $ Link mu $ _targetName rs fp docName
_renderInLink _ _ _	= Nothing

_renderEmbed	:: Cluster -> RenderSettings -> MarkUp -> Maybe MarkUp
_renderEmbed (Cluster docs) rs (Embed docName)
	= Just $ fromMaybe (Parag $ Seq [Base "Document ", imp docName,Base " not found"]) $ do
		doc	<- M.lookup docName docs
		return $ embedder rs doc
_renderEmbed _ _ _	= Nothing


_targetName	:: RenderSettings -> FilePath -> Name -> FilePath
_targetName rs fp nm
	= fp ++ "/" ++ _makeFPproof (renderName rs nm)


-- Removes illegal characters out of names
_makeFPproof	:: String -> FilePath
_makeFPproof	= filter (`notElem` "\\\"'")
