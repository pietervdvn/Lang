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
plural i str	= if i > 1 || i == 0 then number i ++" " ++ pluralize str
			else "one "++ str

-- same as intercalate, but with a empty list filter
intercal	:: String -> [String] -> String
intercal token mds
		= intercalate token $ filter ((/=) "" . strip) mds

pluralize	:: String -> String
pluralize "mouse"	=  "mice"
pluralize "woman"	=  "women"
pluralize "man"		=  "men"
pluralize "gender"	=  "queer"
pluralize "was"		=  "were"
pluralize str		=  str ++ "s"