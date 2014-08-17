module Languate.Signature where

{--
This module implements a function signature, which uniquely identifies functions.
--}
import Languate.FQN
import Languate.AST
import StdDef

data Signature	= Signature Name Type
	deriving (Eq, Ord)


instance Show Signature where
	show (Signature n t)
	 	= tabs 2 n ++ ": "++ show t




signature	:: Function -> [Signature]
signature	=  map (uncurry Signature) . signs


