module Bnf.Loader.Loader where

import Bnf.FQN
import Bnf.Meta.IOModule
import Data.List hiding (lookup, insert)
import Prelude hiding (lookup)
import System.Directory
import Bnf.Meta.Parser
import Bnf.Converter
import Data.Maybe
import Control.Monad
import StateT
import Data.Map (Map, lookup, member, empty, insert, toList)
import System.FilePath
import Data.Tuple
{--


This module implements the loader.

The loader exists of one main loop, which loads files that haven't been loaded yet (and are needed).
The loader is started by asking it to load a FQN from a certain path.
Btw, the loader is quite forgiving about the exact path.
--}


{-- loads all the needed FQN's does not check rules etc...
If the FQN is
bnfs.languate.expression.Constants
and the actual bnf is saved at
/home/pietervdvn/bnfs/languate/expression/Constants.bnf
the load method will find it if the filepath is:
/home/pietervdvn
/home/pietervdvn/bnfs
/home/pietervdvn/bnfs/languate
/home/pietervdvn/bnfs/languate/expression.

Returned: all IOModules imported by this IOModule
-}
load	:: FQN -> FilePath -> IO [(IOModule, FilePath)]
load fqn pwd
	= do	dict <- loadWithImports [(fqn, pwd)] pwd empty
		return $ map swap $ toList dict

{- Loads recursivily modules that aren't loaded yet. Keeps track of the working dir (wherer the program was invoked) and the active dir (where the last module was loaded).-}
loadWithImports	:: [(FQN, FilePath)] -> FilePath -> Map FilePath IOModule -> IO (Map FilePath IOModule)
loadWithImports	[] _ loaded
		= return loaded
loadWithImports	((fqn, lastActiveDir):worklist) workingDir currentlyLoaded
		=  do	path	<- search fqn workingDir (init $ dropFileName lastActiveDir)
			if path `member` currentlyLoaded then loadWithImports worklist workingDir currentlyLoaded
			 else do	modul	<- loadFrom fqn path
					let needed	= zip (map getFQN $ getImports modul) (repeat path)
					loadWithImports (needed++worklist) workingDir $ insert path modul currentlyLoaded

{- Searches the existing path where the FQN could be found. Crashes if no suitable path available-}
search	:: FQN -> FilePath -> FilePath -> IO FilePath
search	fqn pwd cld
	=  do	let paths0	= toFP fqn pwd
		exists0		<- mapM doesFileExist paths0
		let paths1	= toFP fqn cld
		exists1		<- mapM doesFileExist paths1
		let exists	= exists1 ++ exists0
		let paths	= paths1 ++ paths0
		let existing	= nub $ snd $ unzip $ filter fst $ zip exists paths
		if 0 == length existing then
			error $ show fqn ++ " not found starting from '"++pwd ++"', looked at "++show paths++" but none exist (working dir is "++pwd++")"
		 else when (1 /= length existing) $
			putStrLn $ "Warning: multiple "++ show fqn ++" found: "++ show existing ++ "\nThe deepest one is used: "++last existing
		return $ last existing

{- Loads a module from the given location, crashes if errors are found within this bnf file -}
loadFrom	:: FQN -> FilePath -> IO IOModule
loadFrom fqn path
		= do 	putStrLn $ "Loading "++path
			str	<- readFile path
			printErrs fqn path $ parse str





printErrs	:: FQN -> FilePath -> (Maybe IOModule, [Either Warning Error]) ->  IO IOModule
printErrs fqn path (iom, werr)
		= do	found	<- mapM (printErr fqn path) werr
			return $ if or found then error $ "Errors found in "++path++", see stdout for details"
					else	fromJust iom

{- Prints warnings and errors. Returns true if it is an error -}
printErr	:: FQN -> FilePath -> Either Warning Error -> IO Bool
printErr fqn path (Left warning)
		=  do	putStrLn $ "Warning in "++show fqn++" ("++path++")"
			print warning
			return False
printErr fqn path (Right error)
		=  do	putStrLn $ "Error in "++show fqn++" ("++path++")"
			print error
			return True


toFP	:: FQN -> FilePath -> [FilePath]
toFP (FQN paths name) fp
	=  [fp ++ prefixToFP prefix ++ "/" ++ name ++ ".bnf" | prefix <- prefixes paths]

prefixToFP	:: [String] -> String
prefixToFP []	=  []
prefixToFP s	= '/': intercalate "/" s

prefixes	:: [a] -> [[a]]
prefixes as	=  [drop i as | i <- [0..length as] ]



-- testcode
tfqn	= FQN ["exampleBnf"] "Test1"
t	= load tfqn "."
