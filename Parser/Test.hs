module Test where

import Bnf
import Data.Maybe
import Normalizable
import Data.Maybe
import Bnf.ParseTree

{--

This module loads and compiles the bnf's to test them 

--}

pt		:: String -> String -> IO ParseTree
pt rule str	=  do	world	<- load "bnf/Languate"
			print $ length $ show world	-- force evaluation, as to show exceptions
			let pt'	= fromJust $ parse world (toFQN ["Languate"]) rule $ str++"\n"
			let pt  = case pt' of
					Right pt	-> pt
					Left exception	-> error $ show exception
			return $ normalize pt

t rule str	=  pt rule str >>= print . simplify

tr rule str	= pt rule str >>= print


tf		:: FilePath -> IO ()
tf fp		=  do	str 	<- readFile fp
			t "lang" str

main	= t "lang" "123"
