module Languate.MarkUp.MD where

-- This module renders MD stuff
--
import StdDef
import HumanUtils
import State
import Languate.MarkUp.MarkUp
import Languate.MarkUp.Doc
import Data.Map
import Data.List

type MarkDown = String

data MdContext  = MdContext {   titleDepth  :: Int,
                                listDepth   :: Int }

renderMD	:: MarkUp -> State MdContext MarkDown
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
renderMD (NonImp mu)
	= renderMD mu	-- No special options for this in MD, we just render it normally
renderMD (Code mu)
        = renderMD mu |> between "```"
renderMD (Incorr mu)
        = renderMD mu |> between "~~"
renderMD (Titling mu text)
        = do    i     <- get' titleDepth
                title <- renderMD mu |> (replicate i '#' ++)
                setTitleDepth $ i + 1
                text' <- renderMD text
                setTitleDepth i
                return ("\n\n" ++ title ++ "\n\n" ++ text')
renderMD (Link mu s)
        = renderMD mu |> between' "[" "]" |> (++ between' "(" ")" s)
renderMD (Table mus muss)
        = do    header <- mapM renderMD mus
                table  <- mapM (mapM renderMD) muss
                let bars    = intercalate " | "
                let header' = bars header
                let lines   = header ||>> const '-' & bars
                let table'  = table |> bars
                let content = "" : "" : header' : lines : table'
                return $ unlines content
renderMD (List mus)
        = do    i <- get' listDepth
                setListDepth $ i + 1
                list <- mapM renderMD mus   ||>> (++) "* "
                                            ||>> (++) (replicate (i * 3) ' ')
                                            |> unlines
                setListDepth i
                return list
renderMD (OrderedList mus)
        = do    i <- get' listDepth
                setListDepth $ i + 1
                listItems <- mapM renderMD mus
                let list    = zip [1..] listItems
                                |> (\(j, content) -> show j ++ ". " ++ content)
                                |> (++) (replicate (i * 3) ' ')
                                 & unlines & ("\n" ++)
                setListDepth i
                return list
renderMD (InLink mu url)
		= renderMD (Link mu url)
renderMD (Image alt url)
	= do	let txt	= between' "![" "]" alt ++ between' "(" ")" url
		return txt
renderMD (Toggle title conts)
	= renderMD (Titling title conts)
renderMD (Embed title)
	= renderMD (Link (Base $ "Dead link"++title) title)
renderMD (Hover shown footnote)
	= do	shown'	<- renderMD shown
		note'	<- renderMD footnote
		return $ shown' ++ pars (pars note')

renderDoc2MD	:: Doc -> MarkDown
renderDoc2MD dc	= runstate (renderMD $ contents dc) (MdContext 1 0) & fst
--------------- TOOLS ---------------

between	:: String -> MarkDown -> MarkDown
between str md = str ++ md ++ str

between':: String -> String -> String -> String
between' start end str = start++str++end


setTitleDepth   :: Int -> State MdContext ()
setTitleDepth i = do    ctx <- get
                        put $ ctx { titleDepth = i }


setListDepth    :: Int -> State MdContext ()
setListDepth i  = do    ctx <- get
                        put $ ctx { listDepth = i }
