module Languate.TypeTable.Checks.CheckTypeTable where

import StdDef
import HumanUtils
import Exceptions
import Languate.CheckUtils
import Languate.TypeTable
import Languate.TAST

import Data.Map as M

import Control.Arrow

checkTypeTable	:: TypeTable -> Check
checkTypeTable	= checkCurryNumbers

-- we calculate the curry number for each supertype and check wether these are the same
checkCurryNumbers	:: TypeTable -> Check
checkCurryNumbers tt	=
	tt & allSupertypes & M.toList |> (\(typeId, fstt) -> inside ("In the super type table for "++show typeId) $ checkFSTT tt fstt) & sequence_


checkFSTT		:: TypeTable -> FullSuperTypeTable -> Check
checkFSTT tt fstt	=
	do	let curries	= fstt & M.keys |> (curryNumber tt &&& id)
		let msg (nr, rtps)
			= plural nr "argument"++" "++isAre nr++" needed by "++ rtps |> st True & commas
		let msgs	= curries & merge |> msg & unlines
		assert (curries |> fst & allSame) $ "Not all supertypes take the same number of arguments: "++indent ("\n" ++msgs )
