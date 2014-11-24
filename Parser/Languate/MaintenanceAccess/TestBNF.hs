module Languate.MaintenanceAccess.TestBNF where

{--

This module implements code to solely load the bnf and test those

--}

import Bnf
import Normalizable
import Data.Maybe
import Bnf.ParseTree


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
