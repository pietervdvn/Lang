module Languate.Typechecker.TypeTable where

{-

Everythin we know about every type we know of.

This module implements the datastructure in which one can lookup what supertypes a certain type has.

Supertypes can be achieved by implementing an interface, subtype HTML = String,
This means that a type can have multiple, unrelated, direct subtypes.
e.g.
Bool is [Eq, Ord, Monoid, Show, ...]
-}

import Data.Map
import Languate.AST

data TypeTable	= TypeTable	{ supertypes::Map Type [Type]
				, synonyms::Map Type Type
				, classes::[Type]}

allSuperTypes	:: SuperTypeTable -> Type -> [Type]
allSuperTypes table typ
		=  do	direct		<- lookupLst table typ
			indirect	<- allSuperTypes table direct
			return $ nub $ direct ++ indirect
