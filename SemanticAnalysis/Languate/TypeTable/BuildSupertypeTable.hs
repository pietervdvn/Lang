module Languate.TypeTable.BuildSuperTypeTable where

{--

Builds the super type table, which keeps track of what type is instance of what other type.
Note that these types might be partially applied, **with requirements!**

e.g.

instance Set (a:Show) is Show

--}

buildSuperTypeTable	:: World -> SuperTypeTable
