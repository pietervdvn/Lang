module Languate.Checks.CheckSubDef where

import StdDef
import Exceptions

import Languate.AST
import Languate.Checks.CheckUtils
import Languate.Checks.CheckADT
import Languate.Checks.CheckType
import Languate.Checks.CheckComment



validateSubDef tlt (SubDef nm _ frees superTps trex)	-- RAR! T-Rexes are allowed, velociraptors aren't
		= inside ("In the subtype declaration of "++nm) $
			do	validateTypes tlt frees superTps
				validateReqs tlt frees trex
				validateReqsFreeOrder trex frees
