module Languate.MaintenanceAccess.TestPt2AST where

import Bnf
import Normalizable
import Data.Maybe
import Bnf.ParseTree

import Languate.Parser.Pt2Type
import Languate.Parser.Pt2Expr
import Languate.Parser.Pt2Comment
import Languate.Parser.Pt2Pattern
import Languate.Parser.Pt2Law
import Languate.Parser.Pt2Declaration
import Languate.Parser.Pt2Function
import Languate.Parser.Pt2Line
import Languate.Parser.Pt2Import
import Languate.Parser.Pt2DataDef
import Languate.Parser.Pt2TypeDef
import Languate.Parser.Pt2ClassDef
import Languate.Parser.Pt2Statement
import Languate.Parser.Pt2PrecedenceAnnot
import Languate.Parser.Pt2Annot

import Languate.Parser.Pt2Languate

import Languate.File2AST
import System.IO.Unsafe
import StdDef
{--

MAINTANCE ACCESS ONLY!

This module loads and compiles the bnf's to test them.

Usage:

-- test
tst <function to test, e.g. pt2mod> "rule to parse against, e.g. module" "string to parse"

--test file does the same, but on a file, with unsafePerformIO
> tf ... ... "file to read"
e.g.
> tf pt2mod "module" file
This is tf':
> tf'	= tf pt2mod "module", what you'll use for a normal, full module
--}

-- creates a parsetree. Give the rule it should parse against and the string it should parse, you'll get the parsetree
pt		:: String -> String -> IO ParseTree
pt rule str	=  do	world	<- Bnf.load "bnf/Languate"
			let mpt	= parseFull world (toFQN ["Languate"]) rule str
			let pt' = fromMaybe (error "Incorrect parse, not even a single character could be parsed!") mpt
			let pt  = case pt' of
					Right pt	-> pt
					Left exception	-> error $ show exception
			return pt

-- ts rule str	=  pt rule str >>= print . simplify

-- tr rule str	= pt rule str >>= print

tf'		= tf pt2mod "module"

tf		:: (ParseTree -> a) -> Name -> FilePath -> a
tf convertor rule fp
		=  let	str 	= unsafePerformIO $ readFile fp in
			tst convertor rule str


-- generalized test. Give a function which converts a parsetree into something, give a rule, and a string to parse, you'll get the something
tst			:: (ParseTree -> a) -> Name -> String -> a
tst convertor rule str	= convertor (unsafePerformIO $ pt rule str)
