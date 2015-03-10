module Languate.MarkUp.MarkUp (MarkUp (Base, Parag, Seq, Emph, Imp, Code, Incorr, Titling, Link, Table), rewrite, renderMD, renderHTML) where

-- This module implements the base definitions of the markup data structure

import StdDef
import State

import Data.Maybe
import Data.List
-- Represents a snippet of markUpped code
data MarkUp
        = Base String		    -- Embeds a plaintext in markup
        | Parag MarkUp          -- Paragraph for markup
        | Seq [MarkUp]          -- Sequence of markup
	    | Emph MarkUp		    -- Emphasized markup
        | Imp MarkUp 		    -- Important markup
        | Code MarkUp 		    -- Code section
        | Incorr MarkUp 	    -- Incorrect code
        | Titling MarkUp MarkUp -- Embedded titeling [title, markup]
        | Link MarkUp URL       -- A link with overlay text [markup, url]
        | Table [MarkUp] [[MarkUp]]         -- A table [header, tablerows]

type URL = String


rewrite     :: (MarkUp -> Maybe MarkUp) -> MarkUp -> MarkUp
rewrite f mu = fromMaybe (rw f mu) (f mu)


rw          :: (MarkUp -> Maybe MarkUp) -> MarkUp -> MarkUp
rw f (Base str)
            = Base str
rw f (Parag mu)
            = Parag $ rewrite f mu
rw f (Seq mus) 
            = Seq $ mus |> rewrite f
rw f (Emph mu)
            = Emph $ rewrite f mu
rw f (Imp mu)
            = Imp $ rewrite f mu
rw f (Code mu)
            = Code $ rewrite f mu
rw f (Incorr mu)
            = Incorr $ rewrite f mu
rw f (Titling title mu)
            = Titling (rewrite f title) $ rewrite f mu
rw f (Link mu url)
            =  Link (rewrite f mu) url
rw f (Table mus muss)
            = Table (mus |> rewrite f) $ muss ||>> rewrite f

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
             return ("\n\n" ++ title ++ "\n\n"++text')
renderMD (Link mu s)
        = renderMD mu |> between' "[" "]" |> (++ between' "(" ")" s)
renderMD (Table mus muss)
        = do    header <- mapM renderMD mus
                table  <- mapM (mapM renderMD) muss
                let bars = intercalate " | "
                let header' = bars header
                let lines = header ||>> const '-' & bars
                let table' = table |> bars
                let content = "" : header' : lines : table'
                return $ unlines content

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
renderHTML (Table mus muss)
        = do    header <- mapM renderHTML mus
                table  <- mapM (mapM renderHTML) muss
                let header' = header |> inTag' "td" ["id=table_header"] & concat & inTag "tr"
                let table' = table ||>> inTag "td" |> concat |> inTag "tr"
                return $ inTag "table" $ inTag "tbody" $ concat $ header' : table'

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
