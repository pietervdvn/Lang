module Languate.TypeTable.Checks.CheckInstanceDef where

import StdDef
import Exceptions

import Languate.TAST
import Languate.AST
import Languate.CheckUtils
import Languate.TypeTable.Checks.CheckADT
import Languate.TypeTable.Checks.CheckType
import Languate.TypeTable.Checks.CheckKind
import Languate.TypeTable


validateInstance tlt (Instance t@(path, nm) frees super reqs)
	= inside ("In the instance declaration '"++show t++ " is "++show super) $ try err $ do
		fqn	<- findTypeOrigin tlt (path, nm)
		let baseType	= RNormal fqn nm
		let extraFrees	= reqs |> snd >>= freesIn
		let frees'	= extraFrees ++ frees
		validateType tlt frees' super
		validateReqs tlt frees' reqs
