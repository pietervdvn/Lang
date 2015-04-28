module Languate.MarkUp.HTML where

import StdDef
import State
import Languate.MarkUp.MarkUp
import Languate.MarkUp.Doc
import Data.Map hiding (null)

-- This module renders HTML stuff

type HTML     = String

renderHTML	:: MarkUp -> State Int HTML
renderHTML (Base str)
		= return str
renderHTML (Parag mu)
        = mu & rewrite removePars & renderHTML |> inTag "p" |> (++"\n")
renderHTML (Seq mus)
        = mus & mapM renderHTML |> unwords
renderHTML (Emph mu)
		= mu & renderHTML       |> inSpan "emph"
renderHTML (Imp mu)
        = mu & renderHTML       |> inSpan "imp"
renderHTML (NonImp mu)
        = mu & renderHTML       |> inSpan "notImp"
renderHTML (Code mu)
        = mu & renderHTML       |> inSpan "code"
renderHTML (Incorr mu)
        = mu & renderHTML       |> inSpan "incorr"
renderHTML (Titling mu text)
        = do    i <- get
                title <- renderHTML mu |> inTag ("h"++show i)
                put $ i + 1
                text' <- renderHTML text
                put i
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
renderHTML mu
	= return $ inTag "red" $ inTag "b" $ "Markup "++show mu++" can not be rendered to html"

renderDoc2HTML::    Doc -> HTML
renderDoc2HTML doc   = runstate (contents doc & renderHTML) 1 & fst

--------------- TOOLS ---------------

removePars	:: MarkUp -> Maybe MarkUp
removePars (Parag mu)
		= Just $ Seq [mu]
removePars _	= Nothing

inTag   :: String -> HTML -> HTML
inTag tagN html
	= "<" ++ tagN ++ if null html then "/>" else ">" ++ html ++ "</" ++ tagN ++ ">"

inTag'  :: String -> [String] -> HTML -> HTML
inTag' tagN metas html
    = "<" ++ tagN ++ " " ++ unwords metas ++ if null html then "/>" else ">" ++ html ++ "</" ++ tagN ++ ">"

ogpTag  :: Name -> String -> HTML
ogpTag name value
    = "<meta property=\"og:" ++ name ++ "\" content=\"" ++ value ++ "\" />"

inSpan	:: String -> HTML -> HTML
inSpan className html
	= "<span class=\"" ++ className ++ "\">" ++ html ++ "</span>"
