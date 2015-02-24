module Languate.MarkUp.MarkUp (MarkUp (Base, Emph), renderMD, renderHTML) where

-- This module implements the base definitions of the markup data structure

import StdDef

-- Represents a snippet of markUpped code
data MarkUp	= Base String	-- Embeds a plaintext in markup
		| Emph MarkUp	-- Emphasized markup
		| Titling MarkUp MarkUp --Embedded titeling [title, markup]
 

type MarkDown	= String
type HTML	= String

renderMD	:: MarkUp -> MarkDown
renderMD (Base str)
		= str
renderMD (Emph mu)
		= renderMD mu & between "_"

between	:: String -> MarkDown -> MarkDown
between str md = str++md++str


renderHTML	:: MarkUp -> HTML
renderHTML (Base str)
		= str
renderHTML (Emph mu)
		= mu & renderHTML & inDiv "emph"


inTag	:: String -> HTML -> HTML
inTag tagN html
	= "<"++tagN++">"++html++"</"++tagN++">"

inDiv	:: String -> HTML -> HTML
inDiv clas html
	= "<div class=\""++clas++"\">"++html++"</div>"
