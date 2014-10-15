module ParseLine where

{--

This module parses an entire file! 

--}

import Bnf
import Def
import Pt2Stmt

import Control.Monad.Writer
import Data.Maybe

-- notice that this parser parses only one statment
parseLine   :: World -> String -> Statement
parseLine world str
    = 	let parseTree	= parse world (toFQN ["Syntax"]) "statement" str in    -- parses the string with the bnfs in world
	let parseTree'	= fromMaybe (error $ "Oops! We couldn't parse "++show str) parseTree in
	let parseTree''	= (\(Right pt) -> pt) parseTree' in
	fst $ runWriter $ parseStmt parseTree''

parseLines	:: World -> [String] -> [Statement]
parseLines wrld	= map (parseLine wrld)


