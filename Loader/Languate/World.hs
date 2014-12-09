module Languate.World where

{--
This module provides the ''context''-datatype, which contains all commonly needed data of the currently compiling program.
--}

import StdDef
import Data.Map
import Languate.AST
import Languate.FQN
import Data.Set as S
import qualified Data.Map as M

data World	= World 	{ modules	:: Map FQN Module
				, importGraph'	:: Map FQN (Set (FQN, Import))	-- this means: {module --> imports these, caused by this import statement}
				}
	deriving (Show)

importGraph	:: World -> Map FQN (Set FQN)
importGraph w	=  M.map (S.map fst) $ importGraph' w


buildWorld	:: Map FQN (Module, Set (FQN, Import)) -> World
buildWorld dict	=  World (fmap fst dict) (fmap snd dict) 
