module Languate.TypeTable.BuildSuperTT.FixImplicitRequirements where

import Data.Map
import qualified Data.Map as M

import Languate.TAST
import Languate.TypeTable
import Languate.TypeTable.Extended


fixImplicitRequirements	:: Map TypeID FullSuperTypeTable -> Map TypeID FullSuperTypeTable
fixImplicitRequirements fstt
	= fstt


fixImplicitsFor	:: Map TypeID FullSuperTypeTable -> FullSuperTypeTable -> FullSuperTypeTable
fixImplicitsFor fstts
	= fmap (fixImplicitsForEntry fstts)

fixImplicitsForEntry	:: Map TypeID FullSuperTypeTable -> FullSTTEntry -> FullSTTEntry
fixImplicitsForEntry fstts entry@(nameReqs, via, (origType, bnd))
 | not $ isNormal origType	= entry	-- do nothing if the super type is ""a -> b""
 | otherwise
	= entry	-- TODO
