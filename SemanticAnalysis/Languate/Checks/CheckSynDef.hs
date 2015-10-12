module Languate.TypeTable.Checks.CheckSynDef where

import StdDef
import Exceptions

import Languate.TAST
import Languate.AST
import Languate.CheckUtils
import Languate.TypeTable.Checks.CheckADT
import Languate.TypeTable.Checks.CheckType
import Languate.TypeTable.Checks.CheckKind
import Languate.TypeTable


validateSynDef tlt (SynDef nm frees super reqs)
	= inside ("In the synonym declaration 'type "++show nm++ " = "++show super) $ try err $ do
		validateType tlt frees super
		validateReqs tlt frees reqs
