module Languate.FunctionTable.Def where

{--
This module implements the definitions of the functiontable
--}

import Data.Map
import Data.Set
import Languate.AST
import Languate.TAST


data FunctionTable
	= FunctionTable {
		defined	:: Map Signature Visible
		--implementations	:: Map Signature [TClause],
		--documentation	:: Map Signature (String, [Law])
		} deriving (Show)
