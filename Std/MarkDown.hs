module MarkDown where

{- Some things that could be usefull for markdown building -}

import Data.List

type MarkDown	= String


title		:: Int -> String -> MarkDown
title i msg	=  replicate i '#' ++" "++ msg

table		:: [String] -> [[String]] -> MarkDown
table header conts
		= intercalate "\n" $ [bars header, bars $ map (map $ const '-') header] ++ map bars conts

bars	= intercalate " | " 
