module Languate.MarkUp.Options where

import StdDef
import Languate.MarkUp.MarkUp
import Languate.MarkUp.Doc
import Languate.MarkUp.HTML
import Languate.MarkUp.MD

import Control.Applicative
import Control.Arrow

import Data.Map as M
import Data.Set as S
import Data.List as L
import Data.Maybe
-- Implements somme options for the render html, e.g. to add a header, footer, scripts, stylesheet
import Debug.Trace

data RenderSettings	= RenderSettings
	{ renderName	:: String -> (FilePath, URL)
		-- converts the document name to a directory (and links to their correct form). Inlinks are rewritten to links with the URL. Files will be rendered to the given filepath
	, embedder	:: Doc -> MarkUp	-- rendering used to embed (happens before render of course :p)
	, preprocessor	:: Doc -> Doc		-- Invoked before link rewriting and rendering, use this to add headers and footers. It is **not** invoked for embedded rendering
	, render	:: Doc -> String	-- main render function, always used
	, postprocessor	:: Doc -> String -> String
		-- the string is processed with this one right before writing to file
	, overviewPage	:: Maybe ([Doc] -> Doc)	-- generates a nice overview page
	, resources	:: [(String, String)]	-- names of resources, and the content in them
	, resourceName	:: String -> (FilePath, URL)
						-- creates, from a resource name, the url on which it can be found and the path to which it will be rendered. This way, resources don't get lost
	}

-- Represents a postprocessor + needed resource. Resources are always saved in res, relative to the cluster
type Option	= (Doc -> String -> String, [(String, String)])

extend	:: (RenderSettings -> RenderSettings) -> (a -> RenderSettings) -> (a -> RenderSettings)
extend f rs
	= f . rs

vanilla		:: RenderSettings
vanilla	= RenderSettings (id &&& id) (Parag . contents) id (show . Parag . contents)
		(flip const) Nothing [] (id &&& id)

-- Adds a preprocessor and corresponding resources
addOption	:: Option -> RenderSettings -> RenderSettings
addOption (f, res)
	= addPostProcessor f . addResources res


addPreprocessor	:: (Doc -> Doc) -> RenderSettings -> RenderSettings
addPreprocessor fd rs
	= rs {preprocessor = preprocessor rs . fd}

addPreprocessor'	:: (MarkUp -> MarkUp) -> RenderSettings -> RenderSettings
addPreprocessor' fmu
	= addPreprocessor $ preprocess fmu

addPostProcessor	:: (Doc -> String -> String) -> RenderSettings -> RenderSettings
addPostProcessor f rs
	= rs {postprocessor = \doc str -> f doc $ postprocessor rs doc str}

addResources	:: [(String, String)] -> RenderSettings -> RenderSettings
addResources res rs
	= rs {resources = res ++ resources rs}

addResource	= addResources . (:[])

-- Default embedder
fancyEmbedder	:: Doc -> MarkUp
fancyEmbedder doc
	= Parag $ Titling
		(inlink $ title doc) $ Seq
		[Parag $ notImportant $ description doc, Parag $ contents doc]

-- A simple overview page
defaultOverviewPage	:: [Doc] -> Doc
defaultOverviewPage docs
	= let	titl	= "All pages"
		descr	= "Overview of all pages within the cluster"
		(rootDocs, subDocs)	= perDir docs
		subs	= docs |> title & subDirs
		subsFor nm
			= M.findWithDefault [] nm subs |> (\sub -> link' (tail sub) ("#"++escapeURL (tail sub))) & Seq	in
		doc titl descr $ titling titl $
			if M.null subDocs then baseTable rootDocs else Seq
				([ titling' "Root Documents" $ Seq [baseTable rootDocs,
					cleanTable $ table ["Directory","Subdirectories"] $ subDocs & M.keys |> (\nm -> [link' nm $ "#"++escapeURL nm, subsFor nm])]
				] ++ renderSubDirs "" subDocs)

renderSubDirs	:: String -> Map String [Doc] -> [MarkUp]
renderSubDirs superDir dict
 | M.null dict	= []
 | otherwise	= dict & M.mapWithKey (renderSubDir superDir) & M.toList & sortOn fst |> snd

renderSubDir	:: String -> String -> [Doc] -> MarkUp
renderSubDir superDir dir docs
	= let	title'		= drop (1 + length dir + length superDir) . title
		(rootDocs, subDocs)	= perDir' title' docs in
		titling dir $ Seq
			([ baseTable' title' rootDocs ] ++ renderSubDirs (superDir++"/"++dir) subDocs)

baseTable	:: [Doc] -> MarkUp
baseTable 	= baseTable' title

baseTable'	:: (Doc -> String) -> [Doc] -> MarkUp
baseTable' title' docs
	= let	genEntry doc	= [inLink' (title' doc) $ title doc, Base $ description doc] in
		table' ["Title", "Description"] $ docs |> genEntry

-- Sorts the docs in a {"subdir" --> These docs} map + a 'rest' without docs
perDir	:: [Doc] -> ([Doc], Map String [Doc])
perDir	= perDir' title

perDir'	:: (Doc -> String) -> [Doc] -> ([Doc], Map String [Doc])
perDir' title docs
	= let 	(rootDocs, filedDocs)	= docs |> (title &&& id) & L.partition ((notElem '/') . fst) in
		(rootDocs |> snd, filedDocs |> first (fst . break (=='/')) & merge & M.fromList)

subDirs	:: [String] -> Map String [String]
subDirs paths
	= paths & L.filter (elem '/')
		|> reverse
		|> break (=='/') |> snd |> drop 1	-- remove the actual ""file.html""
		|> reverse & L.filter (elem '/')
		|> break (=='/') & merge & M.fromList

defaultNamer	:: FilePath -> URL -> String -> Name -> (FilePath, URL)
defaultNamer fp site ext nm
	= let f p	= p ++ "/" ++ nm ++ ext in
		(f fp, f site)

defaultNamer' fp site
	= defaultNamer fp site ""

localNamer	:: FilePath -> String -> Name -> (FilePath, URL)
localNamer fp
	= defaultNamer fp fp


localNamer' fp	= localNamer fp ""
