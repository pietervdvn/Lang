module Languate.MaintenanceAccess.TestFiles where

{--

tfs = run all files in workspace through the parser

--}

import Languate.MaintenanceAccess.TestPt2AST
import Languate.MaintenanceAccess.TestBNF
import Control.Monad
import System.Directory
import Data.List (isPrefixOf)
import Bnf.ParseTree
import System.IO.Unsafe

import Languate.Parser.Pt2Languate

import Languate.AST

import Data.Set hiding (map, filter)

pfs'	= pfs "../workspace/Data/src"

pfs		:: FilePath -> IO ()
pfs fp		= do	print $ "Loaded bnfs: " ++ show (length $ show world)
			files	<- allFiles fp
			mapM_ pf $ toList files

allFiles	:: FilePath -> IO (Set FilePath)
allFiles fp0	=  do	let fp = if last fp0 == '/' then init fp0 else fp0
			found	<- getDirectoryContents' fp
			let found'	= map (\n -> fp ++ "/" ++ n) found
			files	<- filterM (fmap not . doesDirectoryExist) found'
			dirs	<- filterM doesDirectoryExist found'
			recFiles	<- mapM allFiles dirs
			return $ unions (fromList files:recFiles)


pf	:: FilePath -> IO ()
pf p	=  do	putStr $ "  Parsing file " ++ show p
		m	<- tfio p
		print $ length $ show m
		return ()

ptOfFile	:: FilePath -> IO ParseTree
ptOfFile fp	= do	putStr $ "  Parsing file " ++ show fp
			str	<- readFile fp
			cachedpt "module"  str



getDirectoryContents'	:: FilePath -> IO [FilePath]
getDirectoryContents' fp
	= do	files	<- getDirectoryContents fp
		return $ filter (not . ("." `isPrefixOf`)) files


tfio		:: FilePath -> IO Module
tfio fp		=  do	str	<- readFile fp
			parsetree	<- cachedpt "module"  str
			return $ pt2mod parsetree
