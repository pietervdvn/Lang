module Languate.MarkUp.HTML where

import StdDef
import State
import Languate.MarkUp.MarkUp
import Languate.MarkUp.Doc
import Data.Map hiding (null)

import Data.Char

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
        = mu & renderHTML       |> inTag "strong"
renderHTML (NonImp mu)
        = mu & renderHTML       |> inSpan "notImp"
renderHTML (Code mu)
        = mu & renderHTML       |> inTag "code"
renderHTML (Incorr mu)
        = mu & renderHTML       |> inSpan "incorr"
renderHTML (Titling mu text)
        = do    i <- get
                title <- renderHTML mu |> inTag' ("h"++show i) ["id=\""++escapeURL (toText mu)++"\""]
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
