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


html	:: FilePath -> RenderSettings
html filePath
	= let resF	= localNamer' $ filePath++"/res/" in
	  RenderSettings
		(second escapeURL . localNamer filePath ".html")
		fancyEmbedder
		id
		renderDoc2HTML
		(flip const)
		(Just defaultOverviewPage)
		[]
		resF
	  & addPreprocessor' (rewrite escapeConts)
	  & addOption (headers [titleHeader,  encoding "UTF-8", ogpTags, css resF defaultCSS])



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
css	:: (String -> (String,URL)) -> CSS -> HeaderOption
css resourceF css
	= ([const $ "<link rel=\"stylesheet\" href=\""++ (css & name & resourceF & snd) ++".css\">"
		, const $ inTag "style" $ styleTagConts css]	-- extra headers for the file
		, [(name css++".css", show css)]	-- name and content of css
		)



escapeConts	:: MarkUp -> Maybe MarkUp
escapeConts (Base str)
		= (str >>= escapeChar) & Base & Just
escapeConts _	= Nothing

escapeChar	:: Char -> String
escapeChar c
	| c `notElem` " ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-._~/?#[]@!();=\""
			= "&#x" ++ asHex (ord c) ++  ";"
	| otherwise	= [c]

escapeURL	:: String -> URL
escapeURL str	= str >>= escapeURLChar

escapeURLChar	:: Char -> String
escapeURLChar c
	| c `elem` validURLChars	= [c]
	| otherwise	= "%" ++asHex (ord c)

validURLChars	= "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-._~/?#[]@!$&'()*+,;="

asHex	:: Int -> String
asHex 0	= ""
asHex i	= let 	j	= i `mod` 16
		c	= "0123456789ABCDEF" !! j in
		asHex (i `div` 16) ++ [c]
