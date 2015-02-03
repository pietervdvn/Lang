module Languate.TypeTable.BuildRecursiveSTT where

{--
implements the convertor
--}

import StdDef

import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map, findWithDefault)
import qualified Data.Map as M
import Data.Tuple

import Languate.TAST
import Languate.TypeTable

import Control.Arrow


{- Converts to the recursive type table
Each successive step means a free is filled in. Note that "Any" is not saved as requirement

-}
sttf2rtt	:: SuperTypeTableFor -> RecursiveSuperTypeTable
sttf2rtt	=  mergePaths . buildPaths


{-
Path of requirements:

[
free one should be {Eq, Ord}
free two should be {Show}
]

-}
type Path	= [(Name, Set RType)]


buildPaths	:: Map [Name] (Set (RType, Map Name [RType])) -> [(Path, Set RType)]
buildPaths sttf	=  let paths	= M.toList sttf |> uncurry buildPaths' in
			merge (concat paths) |> (fmap S.unions)

{-
Converts
[k,v] -> { (Set k if k --> Eq); Mappable; Monoid }
into

[ [[Eq], []] --> Set k
  [[], []] --> Mappable, Monoid
]
-}
buildPaths'	:: [Name] -> Set (RType, Map Name [RType]) -> [(Path, Set RType)]
buildPaths' freeNms results
	= let	results'	= S.toList results |> swap
		paths		= map (first $ buildPath freeNms) results' in
		merge paths |> (fmap S.fromList)


buildPath	:: [Name] -> Map Name [RType] -> Path
buildPath frees reqs
	= let	subs n	= filter (/= anyType) $ findWithDefault [] n reqs in
		zip frees $ map subs frees |> S.fromList




-------------
-- MERGING --
-------------

{-
Recursively merges the given paths into a rstt

-}
mergePaths	:: [(Path, Set RType)] -> RecursiveSuperTypeTable
mergePaths paths
	= let	isa	= filter (null . fst) paths |> snd	-- path is empty -> no free to apply -> this type is of given rtypes
		rest	= filter (not . null . fst) paths	-- at least one free to fill in
		-- we split of on the first requirement/free
		rest'	= map (\(a:as, types) -> (a,(as, types))) rest
		-- lets merge those! This way, same type requirements come together
		-- each of the [(as, types)] can be merged further to a rstt
		rest''	= merge rest' |> (fmap mergePaths)
		-- now we have [([reqs for current free], rttf)], which can be put in a dict
		recReqs	= M.fromList rest''	in
		-- we're done
		RTT (S.unions isa) recReqs
