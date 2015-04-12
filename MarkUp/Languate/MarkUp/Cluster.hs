module Languate.MarkUp.Cluster where

import StdDef
import HumanUtils hiding (when)

import Languate.MarkUp.MarkUp
import Languate.MarkUp.Doc
import Languate.MarkUp.Classes
import Data.Map (Map, findWithDefault)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Maybe

import Control.Arrow
import Control.Monad

import System.Directory

-- A cluster is a collection of documents, which can get linked with ILink
data Cluster	= Cluster {docsIn	:: Map Name Doc}

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
	, preprocessor	:: Doc -> Doc
	, postprocessor	:: Doc -> String -> String
	, resources	:: [(String, String)]
	, overviewPage	:: Maybe ([Doc] -> Doc)}



renderClusterTo	:: RenderSettings -> FilePath -> Cluster -> IO ()
renderClusterTo	settings fp (Cluster docsDict)= do
	let docs	= M.elems docsDict
	let docs'	= fromMaybe docs (do	overviewGen	<- overviewPage settings
						return (overviewGen docs':docs))
	let cluster	= docs' |> (title &&& id) & M.fromList
	let cluster'	= cluster |> preprocessor settings
				|> preprocess (rewrite $ _renderEmbed cluster' settings)
				|> preprocess (rewrite $ _renderInLink settings fp)
				& Cluster
	mapM_ (renderFile cluster' settings fp) (docsIn cluster' & M.elems)
	let res		= M.toList (M.fromList $ resources settings)
				|> first ((fp ++ "/res/")++)
	mapM_ (uncurry writeFile') res
	putStrLn $ "Written document cluster to "++fp++" containing "++ show (length docs')++" docs"


renderFile	:: Cluster -> RenderSettings -> FilePath -> Doc -> IO ()
renderFile cluster@(Cluster docs) rs fp doc = do
	let inLinks	= search searchRefs $ contents doc
	let deadLinks	= inLinks & filter (`notElem` M.keys docs)
	unless (null deadLinks) $ putStrLn $ "Warning: the document "++show (title doc)++" contains some dead internal links, namely "++commas deadLinks
	let target	= _targetName rs fp $ title doc
	let str		= postprocessor rs doc $ render rs doc
	writeFile' target str

-- Creates the file on the given path. If the needed directories don't exist, create them
writeFile'	:: FilePath -> String -> IO ()
writeFile' fp contents
	= do	let dirPath	= fp & reverse & break ('/'==) & snd & reverse
		createDirectoryIfMissing True dirPath
		writeFile fp contents

-- Returns all markups with references to different docs for error msgs
searchRefs	:: MarkUp -> Maybe Name
searchRefs (InLink _ nm)
	= Just nm
searchRefs (Embed nm)
	= Just nm
searchRefs _	= Nothing



_renderInLink	:: RenderSettings -> FilePath -> MarkUp -> Maybe MarkUp
_renderInLink rs fp (InLink mu docName)
	= Just $ Link (rewrite (_renderInLink rs fp) mu) $ _targetName rs fp docName
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
