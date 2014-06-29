module Languate.File2AST (loadBnf, parse, load,load') where

{--

This module loads a module from file and makes an AST-module of it. 

--}

import StdDef
import Languate.AST
import qualified Bnf
import Languate.Parser.Pt2Languate
import Data.Maybe


loadBnf		:: FilePath -> IO Bnf.World
loadBnf		=  Bnf.load

-- The bnf-cluster is passed explicitly, they only have to be loaded once.
parse	:: Bnf.World -> FilePath -> String -> Module
parse world path str
		=  let mpt	= Bnf.parseFull world (Bnf.toFQN ["Languate"]) "module" $ str++"\n" in
		   let pt' = fromMaybe (error errMsg) mpt in
		   case pt' of
					Right pt	-> pt2mod pt
					Left exception	-> error $ path++": "++show exception

errMsg	= "Incorrect parse, not even a single character could be parsed!"



load	:: Bnf.World -> FilePath -> IO Module
load bnfs path
	= do	str	<- readFile path
		return $ parse bnfs path str

load'	:: FilePath -> FilePath -> IO Module
load' bnfs path
	=  do	bnfw	<- loadBnf bnfs
		load bnfw path  
