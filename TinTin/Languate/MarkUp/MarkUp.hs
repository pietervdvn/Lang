module Languate.MarkUp.MarkUp (MarkUp (Base, Seq, Emph, Imp, Code, Incorr, Titling, Link), renderMD, renderHTML) where

-- This module implements the base definitions of the markup data structure

import StdDef

-- Represents a snippet of markUpped code
data MarkUp
        = Base String		-- Embeds a plaintext in markup
        | Parag MarkUp      -- Paragraph for markup
        | Seq [MarkUp]      -- Sequence of markup
	    | Emph MarkUp		-- Emphasized markup
        | Imp MarkUp 		-- Important markup
        | Code MarkUp 		-- Code section
        | Incorr MarkUp 	-- Incorrect code
        | Titling MarkUp MarkUp -- Embedded titeling [title, markup]
        | Link MarkUp String 	-- A link with overlay text


type MarkDown	= String
type HTML	= String

renderMD	:: MarkUp -> MarkDown
renderMD (Base str)
		= str
renderMD (Parag mu)
        = renderMD mu ++ "\n"
renderMD (Seq mus)
        = mus |> renderMD & unwords
renderMD (Emph mu)
		= renderMD mu & between "_"
renderMD (Imp mu)
        = renderMD mu & between "**"
renderMD (Code mu)
        = renderMD mu & between "```"
renderMD (Incorr mu)
        = renderMD mu & between "~~"
renderMD (Link mu s)
        = renderMD mu & between' "[" "]" ++ between' "(" ")" s

renderHTML	:: MarkUp -> HTML
renderHTML (Base str)
		= str
renderHTML (Parag mu)
        = mu & renderHTML & inTag "p"
renderHTML (Seq mus)
        = mus |> renderHTML & unwords
renderHTML (Emph mu)
		= mu & renderHTML & inDiv "emph"
renderHTML (Imp mu)
        = mu & renderHTML & inDiv "imp"
renderHTML (Code mu)
        = mu & renderHTML & inDiv "code"
renderHTML (Incorr mu)
        = mu & renderHTML & inDiv "incorr"
renderHTML (Link text link)
        = inTag' "a" ["href="++show link] $ renderHTML text



--------- TOOLS -------------

between	:: String -> MarkDown -> MarkDown
between str md = str++md++str

between':: String -> String -> String -> String
between' start end str = start++str++end

inTag   :: String -> HTML -> HTML
inTag tagN html
	= "<"++tagN++">"++html++"</"++tagN++">"
inTag'  :: String -> [String] -> HTML -> HTML
inTag' tagN metas html
    = "<"++tagN++" "++unwords metas ++">"++html++"</"++tagN++">"

inDiv	:: String -> HTML -> HTML
inDiv clas html
	= "<div class=\""++clas++"\">"++html++"</div>"
