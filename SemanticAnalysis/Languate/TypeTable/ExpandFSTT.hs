module Languate.TypeTable.ExpandFSTT where

{--
This module implements the FSTT expansion
--}

import StdDef


import Languate.TAST
import Languate.TypeTable
import Languate.TypeTable.Bind.Bind (unificate, simpleBind, joinEither)
import Languate.TypeTable.Bind.Substitute

import Prelude hiding (null)
import Data.Set as S hiding (null, filter)
import Data.Map hiding (filter)
import qualified Data.Map as M
import qualified Data.List as L

import Data.Either (rights)
import Data.Tuple
import Data.Maybe

import Languate.TypeTable.Bind.Substitute


{-
Makes the super type table complete, by recursively adding the supertypes of (known) super types.
-}
expand	:: Map TypeID FullSuperTypeTable ->
		(Map TypeID FullSuperTypeTable, Map TypeID SpareSuperTypeTable)
expand fstt
	= (fstt, fmap buildSpareSuperTypeTable fstt)









buildSpareSuperTypeTable	:: FullSuperTypeTable -> SpareSuperTypeTable
buildSpareSuperTypeTable dict
	= dict & keys |> (\a -> (a,a)) ||>> getBaseTID
		|> swap |> unpackMaybeTuple & catMaybes	-- removing failed lookups
		& merge & M.fromList
