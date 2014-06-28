module Languate.TypeChecker where

import Languate.AST
import Languate.SymbolTable


data TypedExpression	= TNat Int
	deriving (Show)
			


-- the context gives the type of each 
typeCheck	:: TypeTable -> Expression -> TypedExpression
typeCheck tt (Nat i)
		= TNat i
