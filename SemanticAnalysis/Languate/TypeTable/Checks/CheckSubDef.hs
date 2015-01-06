module Languate.TypeTable.Checks.CheckSubDef where

import StdDef
import Exceptions

import Languate.AST
import Languate.CheckUtils
import Languate.TypeTable.Checks.CheckADT
import Languate.TypeTable.Checks.CheckType
import Languate.TypeTable.Checks.CheckComment



validateSubDef tlt (SubDef nm _ frees superTps trex)	-- RAR! T-Rexes are allowed, velociraptors aren't
		= inside ("In the subtype declaration of "++nm) $
			do	validateTypes tlt frees superTps
				validateReqs tlt frees trex
				validateReqsFreeOrder trex frees
