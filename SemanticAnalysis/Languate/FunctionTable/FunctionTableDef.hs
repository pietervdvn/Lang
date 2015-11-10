module Languate.FunctionTable.FunctionTableDef where

{--
This module implements the definitions of the functiontable
--}

import Data.Map
import Languate.TAST

data FunctionTable
	= FunctionTable {
		--implementations	:: Map Signature [TClause],
		--documentation	:: Map Signature (String, [Law])
		} deriving (Show)



emptyFT	= FunctionTable
