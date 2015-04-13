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

addOption	:: Option -> RenderSettings -> RenderSettings
addOption f rs	=  rs {postprocessor =
			\doc -> fst f doc . postprocessor rs doc,
			resources	= snd f ++ resources rs}


html	:: RenderSettings
html	= RenderSettings renderDoc2HTML (++".html") fancyEmbedder id id (flip const) []
		(Just defaultOverviewPage) & addOption (headers defaultHeader)

md	:: RenderSettings
md	= RenderSettings renderDoc2MD (++".md") fancyEmbedder id id (flip const) [] (Just defaultOverviewPage)

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


defaultHeader	:: HeaderOption
defaultHeader 	=  [titleHeader, ogpTags] |> headerTag & (cssTag defaultCSS:) & mergeHeaders

headerTag	:: (Doc -> HTML) -> HeaderOption
headerTag f	= ([f],[])


titleHeader	= inTag "title" . title

doctype	= const $ inTag "doctype"

ogpTags	:: Doc -> HTML
ogpTags doc
	= let 	basicOgp ls =  ("title", title doc):("description",description doc) : ls
		ogpTags = meta doc & M.toList & basicOgp |> uncurry ogpTag in
			unlines ogpTags

cssTag	:: CSS -> HeaderOption
cssTag css
	= ([const $ "<link rel=\"stylesheet\" href=\"res/"++ name css ++"\">"
		, const $ inTag "style" $ styleTagConts css]
		, [(name css, show css)])
