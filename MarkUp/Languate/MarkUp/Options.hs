module Languate.MarkUp.Options where

import StdDef
import Languate.MarkUp.MarkUp
import Languate.MarkUp.Doc
import Languate.MarkUp.Cluster
import Languate.MarkUp.HTML
import Languate.MarkUp.MD
import Languate.MarkUp.Css

import Control.Applicative

import Data.Map as M
import Data.Set as S
-- Implements somme options for the render html, e.g. to add a header, footer, scripts, stylesheet

-- Represents a postprocessor + needed resource. Resources are always saved in res, relative to the cluster
type Option	= (Doc -> String -> String, [(String, String)])
type HeaderOption	= ([Doc -> HTML], [(String, String)])

addOption	:: Option -> RenderSettings' -> RenderSettings'
addOption f rs	= rs {postprocessor =
			\doc -> fst f doc . postprocessor rs doc,
			resources	= snd f ++ resources rs}

extend		:: (RenderSettings' -> RenderSettings') -> RenderSettings -> RenderSettings
extend f rs	=  \rf -> f $ rs rf

html	:: RenderSettings
html rf	= RenderSettings renderDoc2HTML (++".html") fancyEmbedder id id (flip const) []
		(Just defaultOverviewPage) rf & addOption (headers (defaultHeader rf))

md	:: RenderSettings
md rf	= RenderSettings renderDoc2MD (++".md") fancyEmbedder id id (flip const) [] (Just defaultOverviewPage) rf

fancyEmbedder doc
	= Parag $ Titling
		(inlink $ title doc) $ Seq [notImportant $ description doc, Parag $ contents doc]

defaultOverviewPage	:: [Doc] -> Doc
defaultOverviewPage docs
	= let	titl	= "All pages"
		descr	= "Overview of all pages within the cluster"
		genEntry doc
			= [inlink $ title doc, Base $ description doc]
		tbl	= table ["Title", "Description"] $ docs |> genEntry in
		doc titl descr $ titling titl tbl


headers	:: HeaderOption -> Option
headers (opts, resources)

	= (\doc body -> "<!DOCTYPE html>" ++ inTag' "html" ["lang=\"en\""] (
		inTag "head" (unlines $ opts <*> [doc]) ++
			inTag "body" (inTag "article" body)), resources)

mergeHeaders	:: [HeaderOption] -> HeaderOption
mergeHeaders headers
		= (headers |> fst & concat, headers |> snd & concat)


defaultHeader	:: (String -> URL) -> HeaderOption
defaultHeader resF
		=  [titleHeader, ogpTags] |> headerTag & (cssTag resF defaultCSS:) & mergeHeaders

headerTag	:: (Doc -> HTML) -> HeaderOption
headerTag f	= ([f],[])


titleHeader	= inTag "title" . title

doctype	= const $ inTag "doctype"

ogpTags	:: Doc -> HTML
ogpTags doc
	= let 	basicOgp ls =  ("title", title doc):("description",description doc) : ls
		ogpTags = meta doc & M.toList & basicOgp |> uncurry ogpTag in
			unlines ogpTags

cssTag	:: (String -> URL) -> CSS -> HeaderOption
cssTag resourceF css
	= ([const $ "<link rel=\"stylesheet\" href=\""++ resourceF (name css) ++"\">"
		, const $ inTag "style" $ styleTagConts css]
		, [(name css, show css)])
