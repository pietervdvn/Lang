module Languate.MarkUp.Cluster where

import StdDef
import Normalizable
import HumanUtils hiding (when)

import Languate.MarkUp.MarkUp
import Languate.MarkUp.Doc
import Languate.MarkUp.Classes
import Languate.MarkUp.Options
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

renderClusterTo	:: RenderSettings -> Cluster -> IO ()
renderClusterTo	settings (Cluster docsDict)= do
	let docs	= M.elems docsDict
	-- new docs dict with the overview page
	let docs'	= fromMaybe docs (do	overviewGen	<- overviewPage settings
						return (overviewGen docs':docs))
	let cluster	= docs' |> (title &&& id) & M.fromList
	-- cluster with documents for embedding
	let embCluster	= cluster
				|> preprocess normalize
				|> preprocess (rewrite $ _renderEmbed embCluster settings)
				|> preprocess (deepRewrite $ _renderInLink settings)
				& Cluster
	let cluster'	= cluster
				|> preprocess normalize
				|> preprocessor settings	-- do the preprocessing (not for embedded values)
				|> preprocess (rewrite $ _renderEmbed embCluster settings)	-- embed all values (from the other cluster)
				|> preprocess (deepRewrite $ _renderInLink settings)
				& Cluster
	-- first render and write resources, as automatic browser reloads already need those
	resources settings	|> first (fst . resourceName settings)
				|> (uncurry writeFile') & sequence_
	renderBar (renderFile cluster' settings) (docsIn cluster' & M.elems)
	putStrLn $ "Written document cluster to "++(renderName settings "" & fst)++" containing "++ show (length docs')++" docs"

renderBar	:: (a -> IO ()) -> [a] -> IO ()
renderBar action ls
		= do	let l	= length ls
			putStr $ bar 80 l 0
			ls & zip [1..] |> (first $ bar 80 l)
				|> (\(bar, a) -> do	putStr $ "\r"++bar
							action a)
				& sequence_
			putStrLn ""


bar		:: Int -> Int -> Int -> String
bar width total current
 | total < current
	= bar width current total
 | otherwise
	= let 	current'= fromIntegral current	:: Float
		total'	= fromIntegral total	:: Float
		width'	= fromIntegral width	:: Float
		perc'	= (current' / total') * (width' - 2)
		perc	= round perc'
		bars	= replicate perc '-'
		spaces	= replicate (width - perc -2) ' ' in
		"["++ bars ++ spaces ++"]"




renderFile	:: Cluster -> RenderSettings -> Doc -> IO ()
renderFile cluster@(Cluster docs) rs doc = do
	let inLinks	= search searchRefs $ contents doc
	let deadLinks	= inLinks & filter (`notElem` M.keys docs)
	unless (null deadLinks) $ putStrLn $ "Warning: the document "++show (title doc)++" contains some dead internal links or embeds, namely "++commas deadLinks
	let target	= _makeFPproof $ fst $ renderName rs $ title doc
	let str		= postprocessor rs doc $ render rs doc
	writeFile' target str

 -- Returns all markups with references to different docs for error msgs
searchRefs	:: MarkUp -> Maybe Name
searchRefs (InLink _ nm)
	= Just nm
searchRefs (Embed nm)
	= Just nm
searchRefs _	= Nothing


_renderInLink	:: RenderSettings -> MarkUp -> Maybe MarkUp
_renderInLink rs (InLink mu docName)
	= do	let url	= renderName rs docName & snd
		return $ Link mu url
_renderInLink _ _	= Nothing

_renderEmbed	:: Cluster -> RenderSettings -> MarkUp -> Maybe MarkUp
_renderEmbed (Cluster docs) rs (Embed docName)
	= do	doc	<- M.lookup docName docs
		return $ embedder rs doc
_renderEmbed _ _ _	= Nothing


-- Removes illegal characters out of names
_makeFPproof	:: String -> FilePath
_makeFPproof	= filter (`notElem` "\\\"")
