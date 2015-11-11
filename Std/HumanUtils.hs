module HumanUtils where

{- Some things humans state in a awkward way -}

import Data.List
import StdDef

import Data.Maybe

commas	= intercalate ", "

pars str	= "("++str++")"

enclose	:: String -> String -> String -> String
enclose op cp md
	| strip md == ""	= ""
	| otherwise	= op ++ md ++ cp


when		:: String -> String -> String
when a b
	| strip b == ""	= ""
	| otherwise	= a ++ b

count	:: Int -> String
count i	= fromMaybe (show i ++"th") $ lookup i $ zip [0..]
		["zeroth","first","second","third","fourth","fifth",
		"sixth","seventh"]

number	:: Int -> String
number 42	= "42 (the answer to life, the universe and everything)"	-- obligatory
number i	= fromMaybe (show i) $ lookup i $ zip [0..]
			["zero", "one", "two","three","four","five","six","seven","eight","nine","ten","eleven","twelve"]

plural	:: Int -> String -> String
plural i str	= number i ++" "++ if i > 1 || i == 0 then pluralize str
			else str

-- same as intercalate, but with a empty list filter
intercal	:: String -> [String] -> String
intercal token mds
		= intercalate token $ filter ((/=) "" . strip) mds

bar		:: Int -> Int -> String -> Int -> String
bar width total msg' current
 | total < current
	= bar width current msg' total
 | otherwise
	= let 	current'= fromIntegral current	:: Float
		total'	= fromIntegral total	:: Float
		width'	= fromIntegral width	:: Float
		perc'	= (current' / total') * (width' - 2)
		perc	= round perc'
		msg	= "--"++take (width - 2) msg'
		preMsg	= take perc msg
		postMsg	= drop perc msg
		bars	= take perc $ preMsg ++ repeat '-'
		conts	= bars++"█"++postMsg++repeat ' '	in
		"["++ take (width-2) conts ++"]"

-- fills i positions after str with char
fill		:: Int -> Char -> String -> String
fill i c str	=  str ++ replicate (i - length str) c


pluralize	:: String -> String
pluralize "mouse"	=  "mice"
pluralize "woman"	=  "women"
pluralize "man"		=  "men"
pluralize "gender"	=  "queer"
pluralize "was"		=  "were"
pluralize "match"	= "matches"
pluralize "pattern match"
			= "pattern matches"
pluralize "is"		= "are"
pluralize "has"		= "have"
pluralize str		=  str ++ "s"

isAre		:: Int -> String
isAre 1		= "is"
isAre _		= "are"
