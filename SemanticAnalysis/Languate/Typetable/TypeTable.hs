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

{-
The type table contains all known types within a certain module.

-}
data TypeTable	= TypeTable	{ known		:: Set (Type, Kind)
				, supertypes	:: Map Type [Type]	-- should have the same kind. E.g. String in List Char; both are *
				, synonyms	:: Map Type Type	-- should have the same kind
				, classes	:: [ClassDef]
				{-
				Tells what functions should be implemented to be an instance of given superclass
				In TypeTable and not in instanceConstr: not a class def!
				-}
				, instConstr	:: Map Type (ClassDef, Kind)}

allSuperTypes	:: SuperTypeTable -> Type -> [Type]
allSuperTypes table typ
		=  do	direct		<- lookupLst table typ
			indirect	<- allSuperTypes table direct
			return $ nub $ direct ++ indirect
