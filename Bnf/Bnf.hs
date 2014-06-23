module Bnf (World, parse,parseFull, load, toFQN, simplify, simpleConvert) where

import System.FilePath
import Bnf.BNF
import qualified Bnf.Loader.Loader as L
import Bnf.Loader.Convertor
import Bnf.PtGen
import Bnf.FQN
import Bnf.ParseTree
import Bnf.Converter (simpleConvert)
{--
This module reexports all needed stuff! 
You'll want to import Bnf.Converter and .ParseTree too!
--}

load	:: FilePath -> IO World
load fp	= do	let name 	= takeBaseName fp
		let wdir	= init $ dropFileName fp
		modules	<- L.load (FQN [] name) wdir
		let world = convert modules
		print $ length $ show world
		return world

toFQN	:: [String] -> FQN
toFQN path
	= FQN (init path) $ last path
