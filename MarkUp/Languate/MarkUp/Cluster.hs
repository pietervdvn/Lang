module Languate.MarkUp.Cluster where

import StdDef
import Normalizable
import HumanUtils hiding (when)

import Graphs.DirectedGraph
import Graphs.SearchCycles


import Languate.MarkUp.MarkUp
import Languate.MarkUp.Doc
import Languate.MarkUp.Classes
import Languate.MarkUp.Options
import Data.Map (Map, findWithDefault)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Maybe
import Data.Tuple

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
	let loops	= searchLoops $ Cluster cluster
	let loopsMsg	= loops |> intercal " --> " & unlines & indent
	if not $ null loops then putStrLn $ "The cluster contains loops withing embedding documents.\n"++loopsMsg else do
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
				|> uncurry writeFile' & sequence_
	missing	<- renderBar (renderFile cluster' settings) (docsIn cluster' & M.elems |> (title &&& id) )
	putStrLn $ "Written document cluster to "++(renderName settings "" & fst)++" containing "++ show (length docs')++" docs"
	unless (null missing) $ putStrLn $ "WARNING: some documents contain dead links or embeds:"++
		(missing & concat & merge >>= missingMsg) & indent

missingMsg	:: (Name, [String]) -> String
missingMsg (title, missing)
	= let	msg	= "\nThe document "++show title++" has "++plural (length missing) "dead link" ++", namely "
		docs	= missing |> show & commas
		in
		msg ++ docs

renderBar	:: (a -> IO b) -> [(String, a)] -> IO [b]
renderBar action ls
		= do	let l	= length ls
			let width	= 79
			putStr $ bar width l "" 0
			bs 	<- zip [1..] ls
					|+> (\(i, (msg,a)) ->  putStr ("\r" ++ bar width l msg i) >> action a)
			putStrLn $ "\r" ++ bar width l (show l++" docs written") l
			return bs


renderFile	:: Cluster -> RenderSettings -> Doc -> IO [(Name, String)]
renderFile cluster@(Cluster docs) rs doc = do
	let inLinks	= search searchRefs $ contents doc
	let deadLinks	= inLinks & filter (`notElem` M.keys docs)
	let target	= _makeFPproof $ fst $ renderName rs $ title doc
	let str		= postprocessor rs doc $ render rs doc
	writeFile' target str
	return $ zip (repeat $ title doc) deadLinks


 -- Returns all markups with references to different docs for error msgs
searchRefs	:: MarkUp -> Maybe Name
searchRefs (InLink _ nm)
	= Just nm
searchRefs (Embed nm)
	= Just nm
searchRefs _	= Nothing

 -- Searches embed-loops
searchLoops	:: Cluster -> [[Name]]
searchLoops cluster
		= let 	graph	= docsIn cluster & M.toList |> second dependsOn & unmerge & fromLinks in
			cleanCycles graph

-- Gives embed-links
dependsOn	:: Doc -> [Name]
dependsOn doc	= contents doc & search embeds
			where 	embeds (Embed n)	= Just n
				embeds _	= Nothing


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
