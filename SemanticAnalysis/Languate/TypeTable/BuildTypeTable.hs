module Languate.TypeTable.BuildTypeTable where

{--
This module builds the type table for the given module. This is done in several steps:


Building of the TYPE LOOKUP TABLE
=================================

-> we build an import table (which states what modules are imported with which aliases) (ImportTable/ImportTable.hs)
-> pass 1 : we build a simple set with what locally declared types (correctness doesn't matter)
	-> we check what types are public (according to the functions)
-> we calculate the export and imports for each module (exportCalculator)
-> Let's build the ''TypeLookupTable''
	-> We can resolve type calls of give an error when ambiguity arises
	-> All this information condenses into the 'TypeLookupTable'


We now have a clear sight which module can view what types, and where this type was originally implemented.


ACTUAL TYPE TABLE
=================

-> Then, we build the class, synonym, instance and subtype defs per module
	-> Error on cycles in the class defs

-> We can now cherrypick the implementation details to build the type table


------

https://www.haskell.org/haskellwiki/GADTs_for_dummies

type synonyms act as functions, with kinds we can do arity checks
data defs and class defs as new declarations (no kind magic here, just simple declarations)
instance defs for predicates
subtype defs for predicates

Type Requirements are passed when needed and implicit

--}
