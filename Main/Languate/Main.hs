module Languate.Main where

{--
Simple function which reads the command line arguments
--}

import StdDef
import System.Environment
import Languate.Tools
import Control.Monad
import Data.Char
import Languate.Pipeline

start	:: IO ()
start	= do	args	<- getArgs
		commands	<- parseArgs args
		runCmds commands (error "Please, load the context first!")



runCmds		:: [(Command, Maybe String)] -> Context -> IO ()
runCmds [] _	=  return ()
runCmds ((cmd, arg):rest) state
		= action cmd state arg (runCmds rest)


-- Parses the command line options. One or two dashes mean: new command, no dash means argument to the previous command
parseArgs	:: [String] -> IO [(Command, Maybe String)]
parseArgs []	= return []
parseArgs (('-':str):rest)
		= do	let name	= str & dropWhile (=='-') |> toLower
			let (args,rest')	= break (\str -> head str == '-') rest
			-- no arguments --> Nothing, otherwise we mash them together
			let args'		= if null args then Nothing else Just $ unwords args
			case command name of
				Nothing	-> putStrLn ("Command '"++name++"' does not exist. See help for list of commands") >> parseArgs rest'
				(Just cmd) -> do	tail	<- parseArgs rest'
							return ((cmd, args'):tail)


repl		:: Context -> IO ()
repl		= -- TODO pickup here
