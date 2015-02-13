module Languate.CheckUtils where

{--
This module implements various recurring things in checks
--}

import Exceptions
import Data.List


type Check	= Exc ()
type Exc a	= Exceptions' String a

-- Checks wether each element in a list occurs exactly once
unique ls	= length ls == (length $ nub ls)

-- Checks wether all the elements in a list are the same
allSame []	= True
allSame (a:as)	= all (==a) as

-- Checks wether or not a list contains exactly one element
isSingleton [_]	= True
isSingleton _	= False




inFile fqn	=  inside $ "In the file "++show fqn
onLine (line, col)
		=  inside $ "On or below line "++show line

onLocation (fqn, coor)
		= inFile fqn . onLine coor

inside str	=  stack' ((++) $ str ++ ":\n")
