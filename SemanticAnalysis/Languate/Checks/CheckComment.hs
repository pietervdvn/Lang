module Languate.Checks.CheckComment where

import StdDef
import Exceptions
import Regex
import ConsumerL
import Data.Maybe
import Data.Char
import Data.List


validateComment' (Just comment)	= validateComment comment
validateComment' _	= pass

validateComment comment
		= let	caseIns	str	= concatMap (\c -> '[':toLower c:toUpper c : "]") str
			choice keyWords	=  (\s -> "(" ++ s ++ ")") $ intercalate "|" $ map caseIns keyWords
			rgx	= regex $ choice ["todo","fixme","fix me","fix-me"] ++"!\n*"
			todos	= mapMaybe (longestMatch rgx) $ lines comment in
			mapM_ warn todos
