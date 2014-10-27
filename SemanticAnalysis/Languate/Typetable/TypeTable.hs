module Languate.Typechecker.TypeTable where

{-

Everythin we know about every type we know of.

The type table keeps track of relations between types.

The supertype keeps track of what type 'implements' what interface
Supertypes can be achieved by implementing an interface, subtype HTML = String,
This means that a type can have multiple, unrelated, direct subtypes.
e.g.
Bool is [Eq, Ord, Monoid, Show, ...]
-}

import Data.Map
import Data.Set
import Languate.AST

data TypeTable	= TypeTable	{ known		:: Set Type
				, supertypes	:: Map Type [Type]
				, synonyms	:: Map Type Type
				, classes	:: [ClassDef]}

allSuperTypes	:: SuperTypeTable -> Type -> [Type]
allSuperTypes table typ
		=  do	direct		<- lookupLst table typ
			indirect	<- allSuperTypes table direct
			return $ nub $ direct ++ indirect
