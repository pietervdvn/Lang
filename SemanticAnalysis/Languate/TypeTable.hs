module Languate.TypeTable where

{--

This module implements a typetable; it provides a {name --> [possibleType]} mapping
Also see [SymbolTable]

--}

import StdDef
import Languate.AST
import Languate.Signature
import Languate.SymbolTable
import Data.Map hiding (map)
import Prelude hiding (lookup)

data TypeTable		= Empt
			| TT {par::TypeTable, cont::Map Name [Type]}
	deriving (Show)

buildTypeTable		:: SymbolTable a -> TypeTable
buildTypeTable Empty	=  Empt
buildTypeTable (Child p cont)
			=  TT (buildTypeTable p) $ fromList $ merge $ map (\(Signature name types) -> (name, types)) $ keys cont


-- The types of the function with given name, closest match first
findType		:: Name -> TypeTable -> Maybe [Type]
findType _ Empt		=  Nothing
findType n (TT p cont)	=  case lookup n cont of
				Nothing -> findType n p
				a	-> a
