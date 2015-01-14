module Languate.FunctionTable where

{--

This module implements a table keeping track of all the functions

--}

import Data.Map
import Prelude hiding (lookup)
import StdDef
import Languate.TAST

type FunctionTable
	= Map Signature FunctionInfo

findExact	:: FunctionTable -> Signature -> Maybe FunctionInfo
findExact ft sign
		=  lookup sign ft
