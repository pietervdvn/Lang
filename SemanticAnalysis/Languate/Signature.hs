module Languate.Signature where

{--
This module implements a function signature, which uniquely identifies functions.
--}
import Languate.FQN
import Languate.AST
import StdDef

data Signature	= Signature Name Type
	deriving (Show, Eq, Ord)

