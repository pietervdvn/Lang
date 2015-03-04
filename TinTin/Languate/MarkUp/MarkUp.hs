module Languate.MarkUp.MarkUp (MarkUp (Base, Emph, Imp, Code, Incorr, Link), renderMD, renderHTML) where

-- This module implements the base definitions of the markup data structure

import StdDef

-- Represents a snippet of markUpped code
data MarkUp	= Base String	-- Embeds a plaintext in markup
		| Emph MarkUp	-- Emphasized markup
        | Imp MarkUp --Important markup
        | Code MarkUp --Code section
        | Incorr MarkUp --Incorrect code
		| Titling MarkUp MarkUp --Embedded titeling [title, markup]
        | Link MarkUp MarkUp --A link


type MarkDown	= String
type HTML	= String

renderMD	:: MarkUp -> MarkDown
renderMD (Base str)
		= str
renderMD (Emph mu)
		= renderMD mu & between "_"
renderMD (Imp mu)
        = renderMD mu & between "**"
renderMD (Code mu)
        = renderMD mu & between "```"
renderMD (Incorr mu)
        = renderMD mu & between "~~"

between	:: String -> MarkDown -> MarkDown
between str md = str++md++str


renderHTML	:: MarkUp -> HTML
renderHTML (Base str)
		= str
renderHTML (Emph mu)
		= mu & renderHTML & inDiv "emph"
renderHTML (Imp mu)
        = mu & renderHTML & inDiv "imp"
renderHTML (Code mu)
        = mu & renderHTML & inDiv "code"
renderHTML (Incorr mu)
        = mu & renderHTML & inDiv "incorr"

inTag	:: String -> HTML -> HTML
inTag tagN html
	= "<"++tagN++">"++html++"</"++tagN++">"

inDiv	:: String -> HTML -> HTML
inDiv clas html
	= "<div class=\""++clas++"\">"++html++"</div>"
