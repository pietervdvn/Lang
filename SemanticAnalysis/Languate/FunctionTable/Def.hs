module Languate.FunctionTable.Def where

{--
This module implements the definitions of the functiontable
--}

import Data.Map
import Data.Set
import Languate.AST
import Languate.TAST


type Generated	= Bool

data FunctionTable
	= FunctionTable {
		defined	:: Map Signature (Visible, Generated)
		--implementations	:: Map Signature [TClause],
		--documentation	:: Map Signature (String, [Law])
		} deriving (Show)
