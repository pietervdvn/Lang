module Languate.MarkUp.HTML where

import StdDef
import State
import Languate.MarkUp.MarkUp
import Languate.MarkUp.Doc
import Data.Map hiding (null)

import Data.Char

-- This module renders HTML stuff

type HTML     = String

data HTMLState	= HTMLState {titleDepth :: Int, usedHeaders :: Int}

renderHTML	:: MarkUp -> State HTMLState HTML
renderHTML (Base str)
		= (str >>= (\c -> if c == '\n' then "<br />" else [c])) & return
renderHTML (Parag mu)
        = mu & rewrite removePars & renderHTML |> (++"<br />\n")
renderHTML (Seq mus)
        = mus & mapM renderHTML |> unwords
renderHTML (Emph mu)
		= mu & renderHTML       |> inSpan "emph"
renderHTML (Imp mu)
        = mu & renderHTML       |> whenTag "strong"
renderHTML (NonImp mu)
        = mu & renderHTML       |> inSpan "notImp"
renderHTML (Code mu)
        = mu & renderHTML       |> whenTag "code"
renderHTML (Incorr mu)
        = mu & renderHTML       |> inSpan "incorr"
renderHTML (Titling mu text)
        = do    i <- get' titleDepth
        	let titleID	= toText mu & space2dash & reverse & break (=='/') & fst & reverse & escapeURL
                let titleLink	= inTag' "a" ["href=\"#"++titleID++"\"", "class=\"anchor\""] "&#x1f517;&nbsp;"
                title <- renderHTML mu |> (titleLink ++ ) |> inTag' ("h"++show i) ["id=\""++titleID++"\""]
                -- the title link is the small icon to link to this subtitle
                -- <a id="user-content-consumer" class="anchor" href="#consumer" aria-hidden="true"><span class="octicon octicon-link"></span></a>
                modify (\htmlstate -> htmlstate {titleDepth = i + 1})
                text' <- renderHTML text
                modify (\htmlstate -> htmlstate {titleDepth = i})
                return $ title ++ text'
renderHTML (Link text link)
        = renderHTML text |> inTag' "a" ["href="++show link]
renderHTML (Table mus muss)
        = do    header <- mapM renderHTML mus
                table  <- mapM (mapM renderHTML) muss
                let header' = header |> inTag' "td" ["class=table_header"] & concat & inTag "tr"
                let table'  = table ||>> inTag "td" |> concat |> inTag "tr"
                return $ inTag "table" $ inTag "tbody" $ concat $ header' : table'
renderHTML (List mus)
        = mapM renderHTML mus ||>> inTag "li" |> concat |> inTag "ul"
renderHTML (OrderedList mus)
        = mapM renderHTML mus ||>> inTag "li" |> concat |> inTag "ol"
renderHTML (InLink mu url)
	= renderHTML $ Link mu url
renderHTML (Image alt url)
	= return $ inTag' "img" ["src="++url, "alt="++show alt] ""
renderHTML (Toggle title conts)
	= do	id	<- get' usedHeaders
		modify (\state -> state {usedHeaders = id + 1})
		title'	<- renderHTML title
		let header	= "toggler"++show id
		let inp	= inTag' "input" ["class=\"toggle-box\"", "id="++ show header, "type=\"checkbox\""] ""
		let label = inTag' "label" ["for=" ++ show header] title'
		conts'	<- renderHTML conts |> inTag "div"
		return (inp ++ label ++ conts')
renderHTML (Embed link)
	= return $ inTag "a" ("Dead embed: "++link)
renderHTML (Hover shown hovertext)
	= do	shown'	<- renderHTML shown
		text'	<- renderHTML hovertext |> inSpan "hover-text"
		return $ inSpan "hover-source" (shown' ++ text')




renderDoc2HTML::    Doc -> HTML
renderDoc2HTML doc   = runstate (contents doc & renderHTML) (HTMLState 1 0) & fst

--------------- TOOLS ---------------

removePars	:: MarkUp -> Maybe MarkUp
removePars (Parag mu)
		= Just $ Seq [mu, Base "\n"]
removePars _	= Nothing

inTag   :: String -> HTML -> HTML
inTag tagN html
	= "<" ++ tagN ++ if null html then "/>" else ">" ++ html ++ "</" ++ tagN ++ ">"

-- renders the tag if not empty
whenTag	:: String -> HTML -> HTML
whenTag tagN html
	= if null $ strip html then "" else inTag tagN html

inTag'  :: String -> [String] -> HTML -> HTML
inTag' tagN metas html
    = "<" ++ tagN ++ " " ++ unwords metas ++ if null html then "/>" else ">" ++ html ++ "</" ++ tagN ++ ">"

ogpTag  :: Name -> String -> HTML
ogpTag name value
    = "<meta property=\"og:" ++ name ++ "\" content=\"" ++ value ++ "\" />"

inSpan	:: String -> HTML -> HTML
inSpan className html
 | null html	= ""
 | otherwise	= "<span class=\"" ++ className ++ "\">" ++ html ++ "</span>"

headerLink str
	= link' str ("#"++escapeURL str)

escapeConts	:: MarkUp -> Maybe MarkUp
escapeConts (Base str)
		= (str >>= escapeChar) & Base & Just
escapeConts _	= Nothing

escapeChar	:: Char -> String
escapeChar c
	| c `notElem` " ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-._~/?#[]@!();=\""
			= "&#x" ++ asHex (ord c) ++  ";"
	| c == '\n'	= "<br />"
	| otherwise	= [c]

space2dash	:: String -> String
space2dash []	= []
space2dash (' ':str)
		= '-':space2dash str
space2dash (s:str)
		= s:space2dash str

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
