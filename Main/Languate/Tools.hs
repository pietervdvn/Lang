module Languate.Tools where

{--

This module implements all the commands and functions to help people with the interpreter.
It also contains all known functions, with help, etc, version number, ...
It contains:

> info "functionName"
which will all it knows (docstring, type, laws, implementation, location)

--}

import StdDef
import HumanUtils
import qualified Languate.Pipeline as Pipeline
import Languate.Pipeline (Context(Context))
import Languate.TAST
import qualified Languate.AST as AST

import qualified Languate.Interpreter as Interpreter
import qualified Languate.Value as Interpreter

import Control.Exception

import Data.Maybe
import Control.Arrow
import qualified Data.Map as M

version	= "0.1.2 \9650 'Black Triangle'"
credits	= ["Made by Pieter Vander Vennet (pietervdvn)","Big thanks to Ilion Beyst for the support","Big thanks to Tlelaxu for occasional coding help"]

data Command	= Command {names	:: [Name]
			  , shortHelp	:: String
			  , longHelp	:: String
			  , action	:: Context -> Maybe String -> (Context -> IO ()) -> IO() 	-- argument, continuation
			  }

continue	:: (a -> IO ()) -> state -> a -> (state -> IO ()) -> IO ()
continue f state a continuation
		= do	f a
			continuation state

continue'	:: (state -> a -> IO ()) -> state -> a -> (state -> IO ()) -> IO ()
continue' f state a continuation
		= do	f state a
			continuation state


instance Show Command where
	show (Command names shortHelp longHelp _)
		= let 	title		= names & commas
			titleLine	= title |> const '-'
			in
			["",title, titleLine, "", indent shortHelp, "", indent longHelp] & unlines

showShort (Command names shortHelp _ _) = (names & commas) ++ "\t " ++ shortHelp


{--} {- The actual commands -} {--} {--} {--} {--} {--} {--} {--} {--} {--} {--} {--} {--} {--} {--}
commands	:: [Command]
commands	= [ help, exit, loadCmd, interpret, buildDocs, info, typeExprCmd, loadBuild, versionCmd, creditsCmd, parseTreeCmd, parseExprCmd, parsePrefExprCmd, parseTExprCmd, showValue]
{--} {--} {--} {--} {--} {--} {--} {--} {--} {--} {--} {--} {--} {--} {--} {--} {--} {--} {--} {--}

command		:: String -> Maybe Command
command name	= commands & filter (\cmd -> name `elem` names cmd) & listToMaybe


loadCmd		= Command ["load","reload","l"] "Loads the prelude and all context" "You should do this before any other command that needs actual code"
			loadCmd'

loadCmd'	:: Context -> Maybe String -> (Context -> IO ()) -> IO()
loadCmd' _ _ cont
		= do	ctx 	<- Pipeline.loadContext
			cont ctx

interpret	= Command ["interpret"] "Interprets given expression, tries to print with 'stringify'" "'stringify' is added in front of your expression"
			$ continue' _interpret

_interpret	:: Context -> Maybe String -> IO ()
_interpret ctx Nothing
		= putStrLn "Hey! We can't interpret if you don't give an expression!"
_interpret ctx@(Context _ tablesOverv) (Just expr)
		= do	tExprs	<- Pipeline.parseTExpr ctx $ "stringify "++ pars expr
			let context	= Interpreter.Ctx tablesOverv Pipeline.prelude M.empty []
			let results	= tExprs |> Interpreter.evalExpr context
			results |> Interpreter.showStringValue & unlines & putStr


info		= Command ["i","info","about","?"] "Gives info about the given function" "Lot's of info!"
				(continue' info')

info'		:: Context -> Maybe String -> IO ()
info' _ Nothing	= putStrLn "What function do you want info about? Type '':info function''"
info' ctx (Just arg)
		= Pipeline.info' ctx arg >>= putStrLn


buildDocs	= Command ["builddocs","bdocs","docs","docgen","tintin"] ("Builds a nice document overview. You'll find your documents in " ++
			Pipeline.path ++ "/index.html") "Your browser should even automatically refresh when you rebuild the docs! And there is some magic in the colors too, especially at night."
			(continue' buildDocs')

buildDocs'	:: Context -> Maybe String -> IO ()
buildDocs' ctx _
		= do	Pipeline.bDocs Pipeline.path ctx
			putStrLn $ "http:///"++Pipeline.path++"/index.html"


loadBuild	= Command ["lb","loadbuilddocs","r"] "Loads the code from disk, then rewrites the docs" "Will become your favorite!"
			_loadBuild


_loadBuild	:: Context -> Maybe String -> (Context -> IO ()) -> IO()
_loadBuild ctx arg cont
		= loadCmd' ctx arg (\ctx -> buildDocs' ctx arg >> cont ctx )

-- compiler dev debug tools, mainly pipeline tools

parseTreeCmd	= Command ["parsetree","pt"] "[advanced] Gives the parsetree of the expression"
			("Makes a tree of tokens, based on the BNF-files. Note that these bnfs are 'dirty loaded' at startup"++
			" and can't be changed at run time")
			(continue $ _argNeeded _parseTreeCmd)
_parseTreeCmd	:: String -> IO ()
_parseTreeCmd expr
		= either putStrLn print $ Pipeline.parseTree expr


parseExprCmd	= Command ["parseexpr","pe"] "[advanced] Gives the parsed expression (not yet typed)"
			"Usefull to debug pt2ast"
			(continue $ _argNeeded _parseExprCmd)
_parseExprCmd	:: String -> IO ()
_parseExprCmd expr
		= either putStrLn print $ Pipeline.parseExpr expr



parsePrefExprCmd	= Command ["parseprefexpr","ppe"] "[advanced] Gives the parsed expression with all operators brought as prefix (not yet typed)"
			"This is usefull to test the precedence of the operators, also see 'Precedence Table' in the documentation!"
			(continue' _parsePrefExprCmd)
_parsePrefExprCmd	:: Context -> Maybe String -> IO ()
_parsePrefExprCmd ctx 	= _argNeeded (either putStrLn print . Pipeline.parsePrefExpr ctx)



parseTExprCmd	= Command ["parsetypedexpr","pte"] "[advanced] Gives the possible typed expressions"
			"This is usefull to test the typing algorithm"
			(continue' _parseTExprCmd)
_parseTExprCmd	:: Context -> Maybe String -> IO ()
_parseTExprCmd ctx expr
		= _argNeeded (Pipeline.parseTExpr ctx) expr >>= print



showValue	= Command ["showvalue","sv"] "[advanced] Shows the internal representation of the function" "Usefull to debug"
			(continue' _showValue)

_showValue	:: Context -> Maybe String -> IO ()
_showValue _ Nothing	= putStrLn "No expression"
_showValue ctx@(Context _ tablesOverv) (Just expr)
	= do	tExprs	<- Pipeline.parseTExpr ctx expr
		let context	= Interpreter.Ctx tablesOverv Pipeline.prelude M.empty []
		let results	= tExprs |> Interpreter.evalExpr context |> show & unlines
		putStrLn results


typeExprCmd	= Command ["type","t"] "Types the given expression"
			"Parses the given expression and tries to type it. If the error message is unclear, ask someone for help"
			(continue' _typeExprCmd)
_typeExprCmd	:: Context -> Maybe String -> IO ()
_typeExprCmd ctx expr
		= do	tExprs	<- _argNeeded (Pipeline.parseTExpr ctx) expr
			tExprs |> typeOf |> show & unlines & ("Possible types are:\n"++) & indent & putStrLn


_argNeeded	:: (String -> IO a) -> Maybe String -> IO a
_argNeeded f (Nothing)	= error "Hey! We need an expression to parse!"
_argNeeded f (Just str)	= f str




-- Housekeeping commands below

versionCmd	= Command ["v","version"] ("Shows the version number (which is "++version++")")
				"If you want to know why the first version is named 'Black Triangle', read rampantgames.com/blog/?p=7745"
				(continue $ const version')
version'	= putStrLn version


creditsCmd	= Command ["credit","credits","author","authors"] "Shows the credits"
				"Credit where credit is due!"
				(continue $ const credit')
credit'		= credits & unlines & putStrLn


exit		= Command ["exit","e","no-repl"] "Exit interpreter or signal not to start the repl when running"
				"Both are totally equivalent"
				(\_ _ _ -> putStrLn "See you soon!")


lazyLoad	= Command [] "Lazy load only loads the context if it's really needed" ""
				lazyLoad'


lazyLoad'	:: Context -> Maybe String -> (Context -> IO ()) -> IO()
lazyLoad' ctx arg cont
		= do	eithCtx <- try (let Context a b = ctx in return $ Context a b)	:: IO (Either SomeException Context)
			ctx'	<- case eithCtx of
					Left e	-> Pipeline.loadContext
					Right c	-> return c
			cont ctx'


help	= Command ["h","help"] "Shows help. Type 'help command' for more info about a specific command"
			"Use 'help *' to get a full overview"
			(continue help')

-- Prints help about a command
help' :: Maybe String -> IO ()
help' Nothing
	= do    ["",
			"Command overview",
			"----------------",
			"",
			"Use 'help *' to get an full outline",
			"Command line arguments are interpreted left to right, and run the commands just the same way as interactive mode.",
			"e.g. if you only want the version number:",
			"lngi -v -e",
			"lngi -version -exit",
			"" ] & unlines & putStrLn
		commands |> showShort & intercal "\n" & putStrLn
		putStrLn ""
help' (Just "*")
	= do	putStrLn "\nCommand outline"
		putStrLn "===============\n"
		version'
		commands |> show & intercal "\n\n" & putStrLn
help' (Just name)
 	= case command name of
		Nothing 	-> putStrLn ("No command '"++name++"' is known. Type ':help' to see all commands")
		(Just command)	-> print command
