module Languate.Checks.CheckUtils where

{--
This module implements various recurring things in checks
--}

import Exceptions
import Data.List


type Check	= Exc ()
type Exc a	= Exceptions' String a

unique ls	= length ls == (length $ nub ls)
isSingleton [_]	= True
isSingleton _	= False


inFile fqn	=  inside $ "In the file "++show fqn
onLine (line, col)
		=  inside $ "On or below line "++show line

onLocation (fqn, coor)
		= inFile fqn . onLine coor

inside str	=  stack' ((++) $ str ++ ":\n")
