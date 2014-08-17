module Languate.Signature where

{--
This module implements a function signature, which uniquely identifies functions.
--}
import Languate.FQN
import Languate.AST
import StdDef
import Normalizable


data Signature	= Signature Name Type
	deriving (Eq, Ord)


instance Show Signature where
	show (Signature n t)
	 	= tabs 2 n ++ ": "++ show t

instance Normalizable Signature where
	normalize (Signature n t)
		= Signature n $ normalize t


signature	:: Function -> [Signature]
signature	=  map (uncurry Signature) . signs


