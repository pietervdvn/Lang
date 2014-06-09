module Test where

import Bnf
import Data.Maybe
import Normalizable
import Data.Maybe

{--

This module loads and compiles the bnf's to test them 

--}

t		:: String -> String -> IO ()
t rule str	=  do	world	<- load "bnf/Languate"
			print $ length $ show world	-- force evaluation, as to show exceptions
			let pt'	= fromJust $ parse world (toFQN ["Languate"]) rule str
			let pt  = case pt' of
					Right pt	-> pt
					Left exception	-> error $ show exception
			print $ simplify $ normalize pt

main	= t "lang" "123"
