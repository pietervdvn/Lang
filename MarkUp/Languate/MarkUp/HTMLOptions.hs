module Languate.MarkUp.HTMLOptions where

{--
This module implements a default RenderSettingsk for html rendering (and options)
--}

import StdDef
import Languate.MarkUp.MarkUp
import Languate.MarkUp.Css
import Languate.MarkUp.Doc
import Languate.MarkUp.HTML
import Languate.MarkUp.Options

import Control.Applicative
import Control.Arrow

import Data.Map as M
import Data.Char


-- Things that go into the header
type HeaderOption	= ([Doc -> HTML], [(String, String)])


html	:: (RenderSettings -> [HeaderOption]) -> RenderSettings -> RenderSettings
html headrs self
	= let filePath = error $ "To use the html options, you should still add a filepath. Use 'fix $ (extend $ setFilePath \"/your/path\") html' (eventually with other options)" in
	   RenderSettings
		filePath
		fancyEmbedder
		id
		renderDoc2HTML
		(flip const)
		(Just defaultOverviewPage)
		[]
		filePath
	 & addPreprocessor' (rewrite escapeConts)
	 & addOption (headers $ headrs self)

defaultHeader	:: CSS -> RenderSettings -> [HeaderOption]
defaultHeader cssFile settings
		= [titleHeader,  encoding "UTF-8", ogpTags, css settings cssFile, reloader settings]

setFilePath	:: FilePath -> RenderSettings -> RenderSettings
setFilePath fp
		= setSite fp "file://"


setSite		:: FilePath -> URL -> RenderSettings -> RenderSettings
setSite fp site self
		= self {renderName = defaultNamer fp  ("file://"++fp) ".html",
			resourceName = let fp' = fp++"/res" in defaultNamer fp' ("file://"++fp') ""
			}


headers	:: [HeaderOption] -> Option
headers options
	= let (opts, resources)	= mergeHeaders options in
		(\doc body ->
		"<!DOCTYPE html>\n" ++ inTag' "html" ["lang=\"en\""] (
		inTag "head" (unlines $ opts <*> [doc]) ++
			inTag "body" (inTag "article" body))
	  , resources)

mergeHeaders	:: [HeaderOption] -> HeaderOption
mergeHeaders headers
		= (headers |> fst & concat, headers |> snd & concat)


headerTag	:: (Doc -> HTML) -> HeaderOption
headerTag f	= ([f],[])


titleHeader	:: HeaderOption
titleHeader	= headerTag (inTag "title" . title)


encoding	:: String -> HeaderOption
encoding enc	= headerTag (const $ inTag' "meta" ["charset=\""++enc++"\""] "")

ogpTags	= headerTag ogpTags'

ogpTags':: Doc -> HTML
ogpTags' doc
	= let 	basicOgp ls =  ("title", title doc):("description",description doc) : ls
		ogpTags = meta doc & M.toList & basicOgp |> uncurry ogpTag in
			unlines ogpTags

-- Adds a css, with the resource function
css	:: RenderSettings -> CSS -> HeaderOption
css self css
	= ([const $ "<link rel=\"stylesheet\" href=\""++ (css & name & resourceName self & snd) ++".css\">"
		, const $ inTag "style" $ styleTagConts css]	-- extra headers for the file
		, [(name css++".css", show css)]	-- name and content of css
		)


reloader	:: RenderSettings -> HeaderOption
reloader self	= ([const $ reloaderheader (snd $ resourceName self "reloader.js")]
			, [("reloader.js", reloaderJS)])

-- yeah, I know... very professional and clean :p
reloaderheader src
		= "<script type=\"text/javascript\" src=\""++src++"\"></script>"
reloaderJS	= "\nvar url\t= document.URL\n\nvar lastVersion = \"\"\n\nvar xhr = new XMLHttpRequest();\nxhr.onreadystatechange = function() {\n    if (xhr.readyState == 4) {\n        console.log(\"Loaded initial version\");\n\tlastVersion = xhr.responseText\n    }\n}\nxhr.open('GET', url, true);\nxhr.send(null);\n\nfunction checkReloadNeeded(text) {\n\tif(text\t== \"\"){\n\t\tconsole.log(\"DOC REMOVED!\")\n\t\tdocument.body.innerHTML = \"<strong>Reloading! Please hang on!</strong>\"\n\t} else if(text != lastVersion){\n\t\tconsole.log(\"RELOAD NEEDED\")\n\t\tlastVersion = text\n\t\twindow.location.reload(true);\n\t}\n}\n\nwindow.setInterval(function(){\n\tvar xhr = new XMLHttpRequest();\n\txhr.onreadystatechange = function() {\n\t    if (xhr.readyState == 4) {\n\t\tcheckReloadNeeded(xhr.responseText)\n\t    }\n\t}\n\txhr.open('GET', url, true);\n\txhr.send(null);\n}, 750);\n"
