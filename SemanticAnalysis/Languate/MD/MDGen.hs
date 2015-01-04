
module Languate.MD.MDGen where

{--

Helper function to generate a markdown overview page.

--}

import StdDef
import MarkDown
import Text.Pandoc
import Text.Blaze.Html.Renderer.Pretty

import Languate.FQN

import Data.Time

-- Embeds with nice footer and header
generate	:: Name -> MarkDown -> IO MarkDown
generate nm md
		=  do	foot	<- footer
			return $ title 1 nm ++ parag md ++ foot

saveTo		:: FilePath -> Name -> IO MarkDown -> IO ()
saveTo fp name md
		=  do	txt	<- md
			writeFile (fp++name++".md") txt
			writeFile (fp++"/html/"++name++".html") $ toHTML txt
			putStrLn $ "Saved markdown and html file to "++fp


toHTML		:: MarkDown -> String
toHTML		=  renderHtml . writeHtml def . readMarkdown def {readerExtensions = githubMarkdownExtensions}


-- generates a footer with "autogenerated on <date>,"...
footer	:: IO MarkDown
footer	= do	time	<- getCurrentTime
		zone	<- getCurrentTimeZone
		let remNanos	= fst . break (=='.')
		let date	= remNanos $ show time
		let dateLocal	= remNanos . show $ utcToLocalTime zone time
		let p =  [parag $ "This page was automatically generated on "++date++" UTC ("++dateLocal++" "++show zone++")"
			, parag $ "Do not edit it, as regeneration will overwrite your changes."
			, "Back to "++link "index" "Index"
			]
		return $ qoute $ unlines p
