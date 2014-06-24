module Test where

import Bnf
import Normalizable
import Data.Maybe
import Bnf.ParseTree

import Def.Parser.Pt2Type
import Def.Parser.Pt2Expr
import Def.Parser.Pt2Comment
import Def.Parser.Pt2Pattern
import Def.Parser.Pt2Law
import Def.Parser.Pt2Declaration
import Def.Parser.Pt2Function
import Def.Parser.Pt2Line
import Def.Parser.Pt2Import
import Def.Parser.Pt2DataDef
import Def.Parser.Pt2TypeDef
import Def.Parser.Pt2ClassDef
import Def.Parser.Pt2Statement

import Def.Parser.Pt2Languate

import Def.File2AST
import System.IO.Unsafe
import StdDef
{--

MAINTANCE ACCESS ONLY!

This module loads and compiles the bnf's to test them.

Usage:

-- test
tst <function to test, e.g. pt2mod> "rule to parse against, e.g. module" "string to parse" 

--test file does the same, but on a file, with unsafePerformIO
tf ... ... "file to read"

--}

-- creates a parsetree. Give the rule it should parse against and the string it should parse, you'll get the parsetree
pt		:: String -> String -> IO ParseTree
pt rule str	=  do	world	<- load "bnf/Languate"
			let mpt	= parseFull world (toFQN ["Languate"]) rule $ str++"\n"
			let pt' = fromMaybe (error "Incorrect parse, not even a single character could be parsed! Check if everything has it's docstring") mpt
			let pt  = case pt' of
					Right pt	-> pt
					Left exception	-> error $ show exception
			return pt

-- ts rule str	=  pt rule str >>= print . simplify

-- tr rule str	= pt rule str >>= print


tf		:: (ParseTree -> a) -> Name -> FilePath -> a
tf convertor rule fp
		=  let	str 	= unsafePerformIO $ readFile fp in
			tst converter rule str


-- generalized test. Give a function which converts a parsetree into something, give a rule, and a string to parse, you'll get the something
tst			:: (ParseTree -> a) -> Name -> String -> a
tst convertor rule str	= convertor (unsafePerformIO $ pt rule str)

