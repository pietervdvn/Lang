module Languate.Main where

{--

This module implements the boot of version 0.0.0 of the languate interpreter.

--}



start	:: IO ()
start	=  welcome





welcome	:: IO ()
welcome	=  putStrLn $ "\nHello and welcome to the Languate Interpreter Network.\n\n"++
	"This interpreter is inbound from sector A 'Source Code' to sector C 'Test Lab'\n"++
	"Please keep in mind that this is but version *0.0.0.1* beta, and contains many bugs.\nTo keep things balanced, it does not contain many features.\n\nHow to use this?\n----------------\n"
