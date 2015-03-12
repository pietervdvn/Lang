module Languate.TypeTable.BuildSuperTT.FixImplicitRequirements where

import Data.Map
import qualified Data.Map as M

import Languate.TypeTable.Extended


import Languate.TypeTable

fixImplicitRequirements	:: Map TypeID FullSuperTypeTable -> Map TypeID FullSuperTypeTable
fixImplicitRequirements fstt
	= fstt


fixImplicitsFor	:: Map TypeID FullSuperTypeTable -> FullSuperTypeTable -> FullSuperTypeTable
fixImplicitsFor fstts
	= fmap (fixImplicitsForEntry fstts)

fixImplicitsForEntry	:: Map TypeID FullSuperTypeTable -> FullSTTEntry -> FullSTTEntry
fixImplicitsForEntry fstts (nameReqs, via, (origType, bnd))
 | not $ isNormal origType	= entry	-- do nothing if the super type is ""a -> b""
 | otherwise
	= let	origTypeTid	= getBaseTID origType
