module Languate.TypeTable.Checks.CheckType where

import StdDef
import Exceptions

import Languate.AST
import Languate.TAST
import Languate.CheckUtils
import Languate.TypeTable.Checks.CheckComment
import Languate.TypeTable

import Data.List


validateReqs tlt frees	= mapM_ (validateType tlt frees . snd)

validateTypes tlt frees	= mapM (validateType tlt frees)
validateType tlt frees t
		= do	rt 	<- catch (resolveType tlt t) (recover t)
			let foundFrees	= freesIn t
			mapM (\a -> assert (a `elem` frees) $ "The free type variable "++show a++" was not declared") foundFrees
			return rt
			where 		recover	t e	= do	err $ "Failed lookup of the type "++show t++".\nContinuing with checks anyway.\nThe error was:\n" ++ e
								return $ RFree "FAILED lookup"
{-
Validates that, when a free is used in a typerequirement, this free is declared and the declaration is on the left of it's usage.
E.g:
data A a (b:X a) is valid, but
data A (a:X b) b	 is not
-}
validateReqsFreeOrder	:: [TypeRequirement] -> [Name] -> Check
validateReqsFreeOrder reqs order
			= mapM_ (_vAllrfo reqs) $ tail' $ inits order

_vAllrfo		:: [TypeRequirement] -> [Name] -> Check
_vAllrfo reqs defined@(free:_)
			= do	let tps	= map snd $ filter ((==) free . fst) reqs
				let stackMsg	= "In the type requirements for "++free++": "++intercalate "," (map show tps)
				inside stackMsg $ mapM_ (_vrfo defined) tps


-- validate that frees in type is declared
_vrfo		:: [Name] -> Type -> Check
_vrfo known t	=  do	let used	= freesIn t
			let faulty	= filter (`notElem` known) used
			let msg a	= "The free type variable "++a++" is not known in the type application of "++show t++".\n Make sure "++a++" is declared before (on the left side) of its usage."
			mapM_ (err . msg) faulty
