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
import Data.Char
-- Implements somme options for the render html, e.g. to add a header, footer, scripts, stylesheet


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
		genEntry doc
			= [inlink $ title doc, Base $ description doc]
		tbl	= table ["Title", "Description"] $ docs |> genEntry in
		doc titl descr $ titling titl tbl

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
