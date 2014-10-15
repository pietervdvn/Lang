module Main where


import Bnf

import Data.List
import ParseLine
import Interpreter

import Data.List

main	:: IO ()
main	=  do	putStrLn "Hello! This is the example program showing how to use the bnf-lib!"
		w	<- world
		f	<- readFile "StraightLine.prog"
		let stms	= parseLines w $ lines f
		putStrLn "The data representing the program is: "
		print stms
		putStrLn "The output of the program is: "
		let printed	= map show $ output stms
		putStrLn $ " " ++ intercalate "\n " printed
		


world	= load "Syntax.bnf"
