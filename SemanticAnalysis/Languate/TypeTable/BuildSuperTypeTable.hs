module Languate.TypeTable.BuildSuperTypeTable where

{--

Builds the super type table, which keeps track of what type is instance of what other type.
Note that these types might be partially applied, **with requirements!**

This table does not verify that types _can be_ subtypes. E.g. instance A is B: buildSuperTypeTable does **not** check wether A implements all the needed functions for B!

e.g.

instance Set (a:Show) is Show

--}

import StdDef

import Exceptions
import Languate.Checks.CheckUtils

import Languate.World
import Languate.TypeTable
import Languate.TAST

import Data.Map
import Data.Set (Set)
import qualified Data.Set as S


import Languate.FQN


{-
type SuperTypeTableFor	= Map [Set RType] (Set RType)
type SuperTypeTable	= Map TypeID SuperTypeTableFor
-}

buildSuperTypeTable	:: World -> Exc SuperTypeTable
buildSuperTypeTable w	=  return $
			    singleton (toFQN' "pietervdvn:Data:Collection.Set","Set") innerMap

innerMap	:: SuperTypeTableFor
innerMap	= singleton ["a"] core

core		:: (Set RType, Map Name [RType])
core		= (S.singleton monoid, singleton "a" [monoid])


monoid	= RNormal (toFQN' "pietervdvn:Data:Category.Monoid") "Monoid"
