module Languate.Main where

{--
Simple function which reads the command line arguments
--}

import StdDef
import System.Environment
import Languate.Tools
import Control.Monad
import Data.Char
import Data.Maybe
import Languate.Pipeline
import System.IO
import System.Console.Readline

import Control.Exception

start	:: IO ()
start	= do	hSetBuffering stdout NoBuffering	-- needed for interactive mode
		hSetBuffering stdin NoBuffering
		commands	<- getArgs |> parseArgs
		runCmds (commands++[(loadCmd, Nothing),(repl, Nothing)]) (error "Please, load the context first!")



runCmds		:: [(Command, Maybe String)] -> Context -> IO ()
runCmds [] _	=  return ()
runCmds ((cmd, arg):rest) state
		= action cmd state arg (runCmds rest)


-- Parses the command line options. One or two dashes mean: new command, no dash means argument to the previous command
parseArgs	:: [String] -> [(Command, Maybe String)]
parseArgs []	= []
parseArgs (str:rest)
		= let	(args,rest')	= break (\str -> head str == '-') rest	-- we get the other arguments which have no '-' in front
			-- no arguments --> Nothing, otherwise we mash them together
			cmdText		= unwords (str:args)
			helpFor str	= (help, Just str)
			cmd		= parseCommand ['-'] helpFor helpFor cmdText	in
			cmd:parseArgs rest'

repl		= Command ["repl"] "REPL-loop" "" (\ctx _ _ -> repl' ctx)

repl'		:: Context -> IO ()
repl' ctx	= repCommand ctx repl'


repCommand		:: Context -> (Context -> IO ()) -> IO ()
repCommand ctx cont
	= do	ln'	<- readline "\9655 "
		let ln	= case ln' of
				Nothing	-> ":exit"
				(Just ln)	-> ln
		if (ln == "") then cont ctx else do
			let (cmd, arg)	= parseCommand [':','-'] (\str -> (interpret, Just str)) (\str -> (help, Just str)) ln
			addHistory ln
			catch (action cmd ctx arg cont) $ errHandle cont ctx



errHandle	:: (Context -> IO ()) -> Context -> SomeException -> IO ()
errHandle cont ctx e
		= do	putStrLn "Something went wrong:"
			print e
			cont ctx

handleSpecials str	= return (++str)

-- Parses a command, which starts with (repetitions of) the given chars, given a default command based on the string
parseCommand	:: [Char] -> (String -> (Command, Maybe String)) -> (String -> (Command, Maybe String)) -> String -> (Command, Maybe String)
parseCommand starts defaultCommand notFoundDefault str
	= let 	(name, args)	= break (==' ') str
		name'	= if head name `elem` starts then Just $ dropWhile (==head name) name else Nothing
		arg	= if args == "" then Nothing else Just args in
		if isNothing name' then defaultCommand str else
			fromMaybe (notFoundDefault name) $ unpackMaybeTuple (name' >>= command, arg)
