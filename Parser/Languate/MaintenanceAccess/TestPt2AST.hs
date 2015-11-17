module Languate.MaintenanceAccess.TestPt2AST where

import Bnf
import Normalizable
import Data.Maybe
import Bnf.ParseTree

import Languate.Parser.Pt2Type
import Languate.Parser.Pt2Annot
import Languate.Parser.Pt2Comment
import Languate.Parser.Pt2Expr
import Languate.Parser.Pt2Law
import Languate.Parser.Pt2Pattern
import Languate.Parser.Pt2Prelude
import Languate.Parser.Pt2Import
import Languate.Parser.Pt2Line
import Languate.Parser.Pt2DataDefSum
import Languate.Parser.Pt2DataDefProd
import Languate.Parser.Pt2DataDef
import Languate.Parser.Pt2Declaration
import Languate.Parser.Pt2Function
import Languate.Parser.Pt2Precedence
import Languate.Parser.Pt2CatDef
import Languate.Parser.Pt2Statement
import Languate.Parser.Pt2Languate
import Languate.Parser.Pt2SubTypeDef
{-
import Languate.File2AST
-}

import System.IO.Unsafe
import StdDef
import Languate.MaintenanceAccess.TestBNF
import Languate.AST
{--

MAINTANCE ACCESS ONLY!

This module loads and compiles the bnf's to test them.

-}
help	= putStrLn "Usage:\ntst <function to test, e.g. pt2mod\n\n> \"rule to parse against, e.g. module\nstring to parse\"\n\ntest file does the same, but on a file, with unsafePerformIO\n> tf ... ... 'file to read'\ne.g.\n> tf pt2mod 'module' file\nThis is tf':\n> tf'\t= tf pt2mod 'module', what you'll use for a normal, full module"

-- creates a parsetree. Give the rule it should parse against and the string it should parse, you'll get the parsetree
-- pt 	:: implemented in TestBNF


ts rule str	=  pt rule str >>= print . simplify
tr rule str	= pt rule str >>= print

-- tf'		= tf pt2mod "module"

tf		:: (ParseTree -> a) -> Name -> FilePath -> a
tf convertor rule fp
		=  let	str 	= unsafePerformIO $ readFile fp in
			tst convertor rule str


-- generalized test. Give a function which converts a parsetree into something, give a rule, and a string to parse, you'll get the something
tst			:: (ParseTree -> a) -> Name -> String -> a
tst convertor rule str	= convertor (unsafePerformIO $ pt rule str)
