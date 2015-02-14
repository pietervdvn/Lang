module MarkDown where

{- Some things that could be usefull for markdown building -}

import Data.List
import StdDef

import Data.Maybe

type MarkDown	= String


title		:: Int -> String -> MarkDown
title i msg	=  replicate i '#' ++" "++ msg++"\n\n"

itemize		:: [MarkDown] -> MarkDown
itemize 	=  parag . unlines . map ("- "++)

table		:: [String] -> [[String]] -> MarkDown
table header conts
	= let 	header'	= bars header
		lines	= header ||>> const '-' & bars
		conts'	= conts & filter (not . null) |> bars in
		unlines (header':lines:conts') ++ "\n\n"

parag ""	= ""
parag str	= str ++ "\n\n"

bars	= intercalate " | "
commas	= intercalate ", "

ital	= modif "_"
bold	= modif "**"
code	= modif "````"
link title to
	= "["++title++"]("++to++")"

firstLine	:: MarkDown -> MarkDown
firstLine str
	= case lines $ stripnl str of
		(s:_)	-> s
		_	-> ""

qoute msg	= "> " ++ intercalate "\n> " (lines msg)

pars str	= "("++str++")"

modif		:: String -> MarkDown -> MarkDown
modif str md
	| strip md == "" =	 ""
	| otherwise	= str ++ strip md ++ str ++ " "

enclose	:: String -> String -> MarkDown -> MarkDown
enclose op cp md
	| strip md == ""	= ""
	| otherwise	= op ++ md ++ cp


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

count	:: Int -> String
count i	= fromMaybe (show i ++"th") $ lookup i $ zip [0..]
		["zeroth","first","second","third","fourth","fifth",
		"sixth","seventh"]

number	:: Int -> String
number 42	= "42 (the answer to life, the universe and everything)"	-- obligatory
number i	= fromMaybe (show i) $ lookup i $ zip [0..]
			["zero", "one", "two","three","four","five","six","seven","eight","nine","ten","eleven","twelve"]

plural	:: Int -> String -> String
plural i str	= if i > 1 || i == 0 then number i ++" " ++ pluralize str
			else "one "++ str

-- same as intercalate, but with a empty list filter
intercal	:: MarkDown -> [MarkDown] -> MarkDown
intercal token mds
		= intercalate token $ filter ((/=) "" . strip) mds

pluralize	:: String -> String
pluralize "mouse"	=  "mice"
pluralize "woman"	=  "women"
pluralize "man"		=  "men"
pluralize "gender"	=  "queer"
pluralize "was"		=  "were"
pluralize str		=  str ++ "s"
