module Languate.TypeTable.Checks.CheckClassDef where

import StdDef
import Exceptions

import Languate.TAST
import Languate.AST
import Languate.CheckUtils
import Languate.TypeTable.Checks.CheckADT
import Languate.TypeTable.Checks.CheckType
import Languate.TypeTable.Checks.CheckKind
import Languate.TypeTable


validateClassDef tlt cd
	= inside ("In the category declaration of "++name cd) $
		do	let supers	= subclassFrom cd
			validateTypes tlt (frees cd) supers
			validateReqs tlt (frees cd) (classReqs cd)
