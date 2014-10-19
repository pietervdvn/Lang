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

classes	:: [PrecRelation] -> [[Name]]
classes	=  union . catMaybes . map sameClass

sameClass	:: PrecRelation -> Maybe (Name, Name)
sameClass (PrecEQ n1 n2)
	| n1 > n2	= Just (n2, n1)
	| otherwise	= Just (n1, n2)
sameClass _	= Nothing


union	:: [(Name, Name)] -> [[Name]]
union list	= todo


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
