module Languate.FunctionTable.Def where

{--
This module implements the definitions of the functiontable
--}
import StdDef

import Data.Map as M
import Data.Set
import Languate.AST
import Languate.TAST
import Languate.FQN


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
	= FunctionTable
		-- what functions are declared in this module?
		{ definedFuncs		:: Map Signature (Visible, Generated, Abstract)
		{- what functions are visible in this module?
			A name can map onto several signatures, type redirection can detect the appropriate one.
			The extra fqns are the fqns of the modules that exported this function, and thus added it to the namespace -}
		, visibleFuncs		:: Map Name (Set (Signature, [FQN]))
		{- Actual implementations of the not abstract methods -}
		, implementations	:: Map Signature [TClause]
		{- Documentation and laws, for humans and correctness checking. Only of the local declared functions -}
		, documentation		:: Map Signature MetaInfo
		} deriving (Show)


emptyFT	:: FunctionTable
emptyFT	= FunctionTable M.empty M.empty M.empty M.empty
