module Languate.MarkUp.HTML where

import StdDef
import State
import Languate.MarkUp.MarkUp
import Languate.MarkUp.Doc
import Data.Map

-- This module renders HTML stuff

type HTML     = String

renderHTML	:: MarkUp -> State Int HTML
renderHTML (Base str)
		= return str
renderHTML (Parag mu)
        = mu & renderHTML       |> inTag "p"
renderHTML (Seq mus)
        = mus & mapM renderHTML |> unwords
renderHTML (Emph mu)
		= mu & renderHTML       |> inDiv "emph"
renderHTML (Imp mu)
        = mu & renderHTML       |> inDiv "imp"
renderHTML (Code mu)
        = mu & renderHTML       |> inDiv "code"
renderHTML (Incorr mu)
        = mu & renderHTML       |> inDiv "incorr"
renderHTML (Titling mu text)
        = do    i <- get
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
                let table'  = table ||>> inTag "td" |> concat |> inTag "tr"
                return $ inTag "table" $ inTag "tbody" $ concat $ header' : table'
renderHTML (List mus)
        = mapM renderHTML mus ||>> inTag "li" |> concat |> inTag "ul"
renderHTML (OrderedList mus)
        = mapM renderHTML mus ||>> inTag "li" |> concat |> inTag "ol"
renderHTML (InLink mu url)
	= renderHTML $ Link mu url

renderDoc2HTML::    Doc -> HTML
renderDoc2HTML doc   = runstate (contents doc & renderHTML) 1 & fst
            
--------------- TOOLS ---------------

inTag   :: String -> HTML -> HTML
inTag tagN html
	= "<" ++ tagN ++ ">" ++ html ++ "</" ++ tagN ++ ">"

inTag'  :: String -> [String] -> HTML -> HTML
inTag' tagN metas html
    = "<" ++ tagN ++ " " ++ unwords metas ++ ">" ++ html ++ "</" ++ tagN ++ ">"

ogpTag  :: Name -> String -> HTML
ogpTag name value 
    = "<meta property=\"og:" ++ name ++ "\" content=\"" ++ value ++ "\" />" 

inDiv	:: String -> HTML -> HTML
inDiv className html
	= "<div class=\"" ++ className ++ "\">" ++ html ++ "</div>"
