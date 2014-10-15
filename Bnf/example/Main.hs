module Main where


import Bnf

import Def
import Data.List
import Interpreter

import Data.List
import Data.Maybe

import Control.Monad.Writer
import Pt2Stmt

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
		
parseTrees	:: IO ()
parseTrees	= do	putStrLn "Hello! This is the example program showing how to use the bnf-lib!"
			w	<- world
			f	<- readFile "StraightLine.prog"
			mapM_ (printPt w) $ lines f

world	= load "Syntax.bnf"
fqn	= toFQN ["Syntax"]

-- notice that this parser parses only one statment
parseLine   :: World -> String -> Statement
parseLine world str
    = 	let parseTree	= parse world fqn "statement" str in    -- parses the string with the bnfs in world
	let parseTree'	= fromMaybe (error $ "Oops! We couldn't parse "++show str) parseTree in
	let parseTree''	= either (error $ "Error: no parse result") id $ parseTree' in
	fst $ runWriter $ parseStmt parseTree''

parseLines	:: World -> [String] -> [Statement]
parseLines wrld	= map (parseLine wrld)


printPt   :: World -> String -> IO ()
printPt world str
    = print $ parse world fqn "statement" str
