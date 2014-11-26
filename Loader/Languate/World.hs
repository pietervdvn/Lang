module Languate.World where

{--
This module provides the ''context''-datatype, which contains all commonly needed data of the currently compiling program.
--}

import Data.Map
import Languate.AST
import Languate.FQN
import Data.Set

data World	= World 	{ modules	:: Map FQN Module
				, importGraph	:: Map FQN (Set FQN)	-- this means: {module --> imports these}
				}
	deriving (Show)


buildWorld	:: Map FQN (Module, Set FQN) -> World
buildWorld dict	=  World (fmap fst dict) (fmap snd dict) 
