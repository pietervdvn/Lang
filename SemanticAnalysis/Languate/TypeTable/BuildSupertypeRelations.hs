module Languate.TypeTable.BuildSypertypeRelations where

{- Way to long package name!

Builds a table (for a module) which states what supertypes are visible, e.g.
module A knows that Set is in Collection.
These kind of relationships are shared aggressively: if A imports B, A knows of types T1 and T2, and B knows of some relationship, A will know about it.

Important: checking wether ""instance"" assignments are correct, is **not** this modules responsability!
-}

import Languate.TypeTable.TypeTable



buildSupertypeTable	:: a
buildSupertypeTable	=  todos "Supertypetable!"

-- typerelation: (A,B) means A is in B.
locallyDeclST	:: [Statement] -> [(RType, RType)]
