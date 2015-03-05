module Languate.MarkUp.MarkUp (MarkUp (Base, Parag, Seq, Emph, Imp, Code, Incorr, Titling, Link), renderMD, renderHTML) where

-- This module implements the base definitions of the markup data structure

import StdDef

import State

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

renderMD	:: MarkUp -> State Int MarkDown
renderMD (Base str)
		= return str
renderMD (Parag mu)
        = renderMD mu |> (++ "\n")
renderMD (Seq mus)
        = mus & mapM renderMD |> unwords
renderMD (Emph mu)
		= renderMD mu |> between "_"
renderMD (Imp mu)
        = renderMD mu |> between "**"
renderMD (Code mu)
        = renderMD mu |> between "```"
renderMD (Incorr mu)
        = renderMD mu |> between "~~"
renderMD (Titling mu text)
        = do i <- get
             title <- renderMD mu |> (replicate i '#' ++) 
             put $ i + 1
             text' <- renderMD text
             put i
             return (title ++ "\n\n"++text')
renderMD (Link mu s)
        = renderMD mu |> between' "[" "]" |> (++ between' "(" ")" s)

renderHTML	:: MarkUp -> State Int HTML
renderHTML (Base str)
		= return str
renderHTML (Parag mu)
        = mu & renderHTML |> inTag "p"
renderHTML (Seq mus)
        = mus & mapM renderHTML |> unwords 
renderHTML (Emph mu)
		= mu & renderHTML |> inDiv "emph"
renderHTML (Imp mu)
        = mu & renderHTML |> inDiv "imp"
renderHTML (Code mu)
        = mu & renderHTML |> inDiv "code"
renderHTML (Incorr mu)
        = mu & renderHTML |> inDiv "incorr"
renderHTML (Titling mu text)
        = do i <- get
             title <- renderHTML mu |> inDiv ("title" ++ show i)
             put $ i + 1
             text' <- renderHTML text
             put i
             return $ title ++ text'
renderHTML (Link text link)
        = renderHTML text |> inTag' "a" ["href="++show link]



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
