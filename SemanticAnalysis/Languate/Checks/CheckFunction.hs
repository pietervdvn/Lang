module Languate.Checks.CheckFunction where


import StdDef
import Exceptions

import Languate.AST
import Languate.Checks.CheckUtils
import Languate.Checks.CheckComment
import Languate.Checks.CheckType
import Languate.Checks.CheckLaw
import Languate.TypeTable

import Data.List


validateFunction	:: TypeLookupTable -> Function -> Check
validateFunction tlt (Function docStr _ signs laws clauses)
		= inside ("In the function declaration of "++intercalate ", " nms) $
		   do	mapM_ validateLaw laws
			mapM_ (validateSign tlt) signs
			validateComment docStr
			where nms	= nub $ map (\(n,_,_) -> n) signs




validateSign	:: TypeLookupTable -> (Name, Type, [TypeRequirement]) -> Check
validateSign tlt (name, t, tr)
		=  do	validateType tlt (freesIn t) t
			validateReqs tlt (freesIn t) tr
