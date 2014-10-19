module Languate.Precedence.BuildPrecTable where

{--
This module builds the precedence table, step by step.
--}

import StdDef
import State
import Languate.AST

import Data.Map hiding (union, map)
import Data.Maybe
import Prelude hiding (lookup)

import Debug.Trace

-- ### Checks

{- checks if a ''(+) < (-)'' relation does not exist if ''(+) = (-)'' is defined somewhere. Thus, if two operators are in the same union, checks no LT-relation exists between them.
Returns faulty arguments.
-}
checkIllegalLT	:: [(Name, Name)] -> Map Name Name -> [(Name,Name)]
checkIllegalLT ltRels dict
		=  fst $ runstate (mapM checkIllegalLT' ltRels  >>= return . catMaybes) dict

checkIllegalLT'	:: (Name, Name) -> State (Map Name Name) (Maybe (Name,Name))
checkIllegalLT' ops@(o1, o2)
		=  do	r1	<- representative o1
			r2	<- representative o2
			return  $ if r1 == r2 then Just ops	-- the returned representatives are the same, meaning o1 and o2 are in the same union; however an o1 < o2 relation exists too. ERROR
						else Nothing


-- ### Union find algorithm

union'	:: [(Name, Name)] -> Map Name Name
union' tuples
	= snd $ runstate (mapM_ add tuples) empty

-- the map is a map of pointers. A name will always try to point to its representative, being the smallest element of the set.
-- State invariant: if the name appears as value in the map, it will also appear as key.
add	:: (Name, Name) -> State (Map Name Name) ()
add (n1,n2)	= do	repres1	<- representative n1
			repres2	<- representative n2
			let r	= if repres1 < repres2 then repres1 else repres2
			modify $ insert n1 r
			modify $ insert n2 r
			modify $ insert repres1 r
			modify $ insert repres2 r


-- searches the representative for the given name. Does never insert nm in the map.
representative	:: Name -> State (Map Name Name) Name
representative nm
		= do 	dict	<- get
			let nmv	= lookup nm dict
			case nmv of
				(Just found)	-> collapse nm found
				(Nothing)	-> do	return nm


collapse	:: Name -> Name -> State (Map Name Name) Name
collapse nm found
	| nm == found	= return nm
	| otherwise	= do	repres	<- representative found
				modify	$ insert found repres
				return repres
