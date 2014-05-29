module Test where

import Bnf
import Data.Maybe

{--

This module loads and compiles the bnf's to test them 

--}

t	:: String -> IO ()
t str	=  do	world	<- load "bnf/Languate"
		let pt	= parse world (toFQN ["Languate"]) "int" str
		print pt

main	= t "123"
