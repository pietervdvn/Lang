module Languate.TypeTable.Checks.CheckType where

import StdDef
import Exceptions
import Prelude hiding (catch)
import Languate.AST
import Languate.TAST
import Languate.CheckUtils
import Languate.TypeTable

import Data.List


validateReqs tlt frees	= mapM_ (validateType tlt frees . snd)

validateType' tlt t	= validateType tlt (freesIn t) t

validateTypes tlt frees	= mapM (validateType tlt frees)
validateType tlt frees t
		= do	rt 	<- catch (resolveType tlt t) (recover t)
			let foundFrees	= freesIn t
			mapM_ (\a -> assert (a `elem` frees) $ "The free type variable "++show a++" was not declared") foundFrees
			return rt
			where 		recover	t e	= do	err $ "Failed lookup of the type "++show t++".\nContinuing with checks anyway.\nThe error was:\n" ++ e
								return $ RFree "FAILED lookup"
