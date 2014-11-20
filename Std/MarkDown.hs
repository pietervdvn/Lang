module MarkDown where

{- Some things that could be usefull for markdown building -}

import Data.List
import StdDef

type MarkDown	= String


title		:: Int -> String -> MarkDown
title i msg	=  replicate i '#' ++" "++ msg++"\n\n"

itemize		:: [MarkDown] -> MarkDown
itemize 	=  parag . unlines . map ("- "++)

table		:: [String] -> [[String]] -> MarkDown
table header conts
		= (intercalate "\n" $ [bars header, bars $ map (map $ const '-') header] ++ map bars conts) ++ "\n\n"

parag ""	= ""
parag str	= str ++ "\n\n"

bars	= intercalate " | "
commas	= intercalate ", "

ital	= modif "_"
bold	= modif "**"
code	= modif "````"

modif		:: String -> MarkDown -> MarkDown
modif str md
	| strip md == "" =	 ""
	| otherwise	= str ++ strip md ++ str ++ " "


when		:: MarkDown -> MarkDown -> MarkDown
when a b
	| strip b == ""	= ""
	| otherwise	= a ++ b

-- replaces each '' by ```` , as '' is easier to type
recode		:: String -> MarkDown
recode []	= []
recode ('\'':'\'':str)
		= "````"++recode str
recode (a:str)	= a:recode str
