module Classes where

-- A small class experiment

import Data.Map as Map


type Flip x a b	= x b a

class Coll coll where
	getAll	:: coll a -> [a]

instance Coll (Flip Map a) where
	getAll dict	= keys dict


empt	:: Flip Map Int String
empt 	=  empty
