module Languate.Checks.CheckUtils where

{--
This module implements various recurring things in checks
--}

import Exceptions
import Data.List


type Check	= Exceptions' String ()

unique ls	= length ls == (length $ nub ls)

inFile fqn	=  inside $ "In the file "++show fqn
onLine (line, col)
		=  inside $ "On or below line "++show line

inside str	=  stack' ((++) $ str ++ ":\n")
