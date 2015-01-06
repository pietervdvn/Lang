module Languate.TypeTable.Checks.CheckFunction where


import StdDef
import Exceptions

import Languate.AST
import Languate.CheckUtils
import Languate.TypeTable.Checks.CheckComment
import Languate.TypeTable.Checks.CheckType
import Languate.TypeTable.Checks.CheckLaw
import Languate.TypeTable

import Data.List


validateFunction	:: TypeLookupTable -> Function -> Check
validateFunction tlt (Function _ signs clauses)
		= inside ("In the function declaration of "++intercalate ", " nms) $
			mapM_ (validateSign tlt) signs
			where nms	= nub $ map (\(n,_,_) -> n) signs




validateSign	:: TypeLookupTable -> (Name, Type, [TypeRequirement]) -> Check
validateSign tlt (name, t, tr)
		=  do	validateType tlt (freesIn t) t
			validateReqs tlt (freesIn t) tr
