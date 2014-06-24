module Languate.File2AST (loadBnf, parse, load) where

{--

This module loads a module from file and makes an AST-module of it. 

--}

import StdDef
import Languate.AST
import qualified Bnf
import Languate.Parser.Pt2Languate
import Data.Maybe


loadBnf		:: IO Bnf.World
loadBnf		=  Bnf.load "bnf/Languate"

-- The bnf-cluster is passed explicitly, they only have to be loaded once.
parse	:: Bnf.World -> String -> Module
parse world str
		=  let mpt	= Bnf.parseFull world (Bnf.toFQN ["Languate"]) "module" $ str++"\n" in
		   let pt' = fromMaybe (error errMsg) mpt in
		   case pt' of
					Right pt	-> pt2mod pt
					Left exception	-> error $ show exception

errMsg	= "Incorrect parse, not even a single character could be parsed! Check if everything has it's docstring"



load	:: Bnf.World -> FilePath -> IO Module
load bnfs path
	= do	str	<- readFile path
		return $ parse bnfs str
