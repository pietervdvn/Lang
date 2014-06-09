module Bnf (World, parse, load, toFQN, simplify) where

import System.FilePath
import Bnf.BNF
import qualified Bnf.Loader.Loader as L
import Bnf.Loader.Convertor
import Bnf.PtGen
import Bnf.FQN
import Bnf.ParseTree
{--
This module reexports all needed stuff! 
You'll want to import Bnf.Converter and .ParseTree too!
--}

load	:: FilePath -> IO World
load fp	= do	let name 	= takeBaseName fp
		let wdir	= init $ dropFileName fp
		modules	<- L.load (FQN [] name) wdir
		return $ convert modules

toFQN	:: [String] -> FQN
toFQN path
	= FQN (init path) $ last path
