module Languate.FunctionTable.Def where

{--
This module implements the definitions of the functiontable
--}
import StdDef

import Data.Map as M
import Data.Set
import Languate.AST
import Languate.TAST


type Generated	= Bool
type Abstract	= Maybe RType

data MetaInfo	= MetaInfo
			{ metaDefined	:: Coor
			, metaLaws	:: [(Law	, Coor)]
			, metaComments	:: [(Comment	, Coor)]
			, metaDocs	:: [(((Name, Name), Comment), Coor)]
			, metaAnnots	:: [(Annotation	, Coor)]
			} deriving (Show)

data FunctionTable
	= FunctionTable {
		defined	:: Map Signature (Visible, Generated, Abstract)
		--implementations	:: Map Signature [TClause],
		, documentation	:: Map Signature MetaInfo -- , [Law])
		} deriving (Show)


emptyFT	:: FunctionTable
emptyFT	= FunctionTable M.empty M.empty
