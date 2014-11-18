module Languate.Main where

{--

This module implements the boot of version 0.0.0 of the languate interpreter.

--}

import StdDef
import qualified Bnf
import Data.Maybe
import Languate.FQN
import Languate.TypedLoader
import Languate.ReadEvalPrint
import Languate.TypedPackage
import Languate.Tools
import Languate.Precedence.Precedence

import System.Directory
import Prelude hiding (catch)
import Control.Exception (catch, SomeException)

import Data.List (isPrefixOf)
import Data.Map (findWithDefault)

import Languate.Pipeline
import Data.Time.Clock

import System.Environment
import Control.Monad

version	= "0.0.0.0.4"


start	:: IO ()
start	=  do	welcome
		putStrLn $ "Loading bnf-files from "++bnfPath
		(pack, bnfs, precT)	<- doAllStuff
		args	<- getArgs
		if "--no-repl" `elem` args then
			putStrLn "All done!"
		else do  putStrLn' "All done!"
			 repl bnfs pack precT prelude


repl	:: Bnf.World -> TPackage -> PrecedenceTable -> FQN -> IO ()
repl w tp precT fqn
	=  do 	line	<- getLine
		repl' w tp precT fqn line


repl'	:: Bnf.World -> TPackage -> PrecedenceTable -> FQN -> String -> IO ()
repl' _ _ _ _ "--exit"	= putStrLn "Bye!"
repl' w tp precT fqn line
	| "--i" `isPrefixOf` line
		|| "\EOT" == line
			= do	printInfo tp fqn $ drop 4 line
				repl w tp precT fqn
	| "\f" == line	= putStrLn "\f" >> repl w tp precT fqn
	| "--p" `isPrefixOf` line
			= do 	print $ parseExpr w precT $ drop 4 line
				repl w tp precT fqn
	| otherwise	= do	rep w tp precT fqn line
				repl w tp precT fqn

-- one step
rep	:: Bnf.World -> TPackage -> PrecedenceTable -> FQN -> String -> IO ()
rep bnfs tpack precT fqn str
	= do	let evaled = parseEval bnfs tpack precT fqn str
		catch (putStrLn' $ show evaled) hndl



hndl	:: SomeException -> IO ()
hndl msg	= putStrLn' $ "Hi! Something went wrong with that statement!\n"++show msg


putStrLn'	:: String -> IO ()
putStrLn' str	=  putStr $ str++"\n> "
printInfo	:: TPackage -> FQN -> Name -> IO ()
printInfo tp fqn name
		=  do	let tmod	= findWithDefault (error $ show fqn++" not found!") fqn tp
			putStrLn' $ info tmod name


welcome	:: IO ()
welcome	=  do	time	<- getCurrentTime
		putStrLn $ msg $ show time

msg	:: String -> String
msg time
	= "\nGood morning, and welcome to the Languate Interpreter System."++
		"\nThis automated interpreter is provided for for the security and\n"++
		"convenience of the Languate Research personnel.\n"++
		"The time is "++time++", current topside temperature is 91 degrees with an estimated high of 105.\n"++
		"This interpreter is inbound from Level 3 'Source Code' to sector C 'Test Labs and Control Facilities'.\n\n"++
	"Please keep in mind that this is but version *"++version++"* beta, and contains many bugs.\nTo keep things balanced, it does not contain many features.\n\nHow to use this?\n----------------\n\n"++
	"This version is very limited, and will load the 'Data' project in 'workspace' ("++project++"). More precisely, it will load the 'prelude'.\n\nYou can evaluate expressions by typing them;\n --i <functionname> gives you all the information about the function;\n--p <expression> parses the expression and shows it, which shows how infix expressions are parsed.\n\nDo not expect a lot, espacially clear and descriptive error messages are rare.\n\nTake little steps.\n\nWork safe, work smart.\nNow arriving at at Sector C Test Labs. Have a very safe and productive day."

up	= setCurrentDirectory ".."
t	= up >> start
