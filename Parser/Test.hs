module Test where

import Bnf
import Data.Maybe

{--

This module loads and compiles the bnf's to test them 

--}

t		:: String -> String -> IO ()
t rule str	=  do	world	<- load "bnf/Languate"
			print $ length $ show world	-- force evaluation, as to show exceptions
			let pt	= parse world (toFQN ["Languate"]) rule str
			print pt

main	= t "lang" "123"
