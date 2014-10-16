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

import System.Directory
import Prelude hiding (catch)
import Control.Exception (catch, SomeException)

import Data.List (isPrefixOf)
import Data.Map (findWithDefault)

version	= "0.0.0.0.1"

bnfPath	= "Parser/bnf/Languate"
project	= "workspace/Data/src/"
fqpn		= fromJust $ toFQPN "pietervdvn:Data"
prelude		= fromJust $ toFqn' fqpn [] "Prelude"
bool		= fromJust $ toFqn' fqpn ["Data"] "Bool"




start	:: IO ()
start	=  do	welcome
		putStrLn $ "Loading bnf-files from "++bnfPath
		bnfs	<- Bnf.load bnfPath
		pack	<- typedLoad bnfs prelude project
		putStrLn' "All done!"
		repl bnfs pack prelude


repl	:: Bnf.World -> TPackage -> FQN -> IO ()
repl w tp fqn
	=  do 	line	<- getLine
		repl' w tp fqn line


repl'	:: Bnf.World -> TPackage -> FQN -> String -> IO ()
repl' _ _ _ "--exit"	= putStrLn "Bye!"
repl' w tp fqn line
	| "--i" `isPrefixOf` line
		|| "\EOT" == line
			= do	printInfo tp fqn $ drop 4 line
				repl w tp fqn
	| "\f" == line	= putStrLn "\f" >> repl w tp fqn
	| otherwise	= do	rep w tp fqn line
				repl w tp fqn	

-- one step
rep	:: Bnf.World -> TPackage -> FQN -> String -> IO ()
rep bnfs tpack fqn str
	= do	let evaled = parseEval bnfs tpack fqn str
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
welcome	=  putStrLn $ "\nHello and welcome to the Languate Interpreter Network.\n\n"++
	"This interpreter is inbound from sector A 'Source Code' to sector C 'Test Lab'\n"++
	"Please keep in mind that this is but version *"++version++"* beta, and contains many bugs.\nTo keep things balanced, it does not contain many features.\n\nHow to use this?\n----------------\n\n"++
	"This version is very limited, and will load the 'Data' project in 'workspace' ("++project++"). More precisely, it will load the 'prelude'.\n\nYou can evaluate expressions by typing them; and ask information about functions with --i <functionname>.\n\nDo not expect a lot, espacially clear and descriptive error messages (take little steps).\n\nGood luck!\n\n"

up	= setCurrentDirectory ".."
t	= up >> start

