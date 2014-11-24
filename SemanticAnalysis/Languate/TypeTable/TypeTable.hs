module Languate.TypeTable.TypeTable where

{-

Everythin we know about every type we know of.

The type table keeps track of relations between types.

The supertype keeps track of what type 'implements' what interface
Supertypes can be achieved by implementing an interface, subtype HTML = String,
This means that a type can have multiple, unrelated, direct subtypes.
e.g.
Bool is [Eq, Ord, Monoid, Show, ...]
-}

import StdDef
import Data.Map
import Data.Set
import Languate.AST
import Languate.TAST
import Languate.FQN

{-
The type table contains all known types within a certain module.

-}

data Duplicate	= Duplicate [FQN]
data TypeTable	= TypeTable	{ known		:: Map (FQN, Type) (Kind, Set TypeRequirement)	-- type requirements are implicit; contains synonyms
				, supertypes	:: Map Type (Set Type)	-- direct super types. should have the same kind. E.g. String in List Char; both are *
				, synonyms	:: Map Type Type	-- should have the same kind. Acts as an 'equivalence/equality' relation
				, revSynonyms	:: Map Type (Set Type)	-- inverse relation of synonyms
				{-
				Tells what functions should be implemented to be an instance of given superclass
				In TypeTable and not in instanceConstr: not a class def!
				-}
				, instConstr	:: Map Type (ClassDef, Kind)}
	deriving (Show, Ord, Eq)

type TypeLookupTable	= Map ([Name], Name) [(FQN, Visible)]	-- mutliple values, multiple possiblities in some cases!

{-

Type requirements propagate implicitly.
E.g:

    data Set (a:Eq) = <details>

    f : Set a -> a -> ...

The type requirement that ''a'' should be in ''Eq'' is known, but should not be stated explicitly.



-}
