module StdDef where

import Data.Tuple
import Data.Map (Map, insert, adjust, findWithDefault)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Map (Map)
import Data.Set (Set)

type Name 	= String
type FileName	= String
type LineNumber = Int
type CharNumber	= Int
type Pos	= Int
type Position	= (LineNumber, CharNumber)

todo	= error "TODO"
todos	= error . (++) "TODO: "

const2		:: a -> b -> c -> a
const2 a _ _ 	=  a

-- non crashing version of init
init'		:: [a] -> [a]
init' []	=  []
init' ls	=  init ls

-- non crashing version of tail
tail'		:: [a] -> [a]
tail' []	=  []
tail' ls	=  tail ls

-- takes the last element of the list; if the list is empty, take the default
last'		:: a -> [a] -> a
last' defaul []	=  defaul
last' _ ls	=  last ls


dubbles		:: Eq a => [a] -> [a]
dubbles []	=  []
dubbles (a:as)	=  (if a `elem` as then (a:) else id) $ dubbles as


all2	:: (a -> b -> Bool) -> [a] -> [b] -> Bool
all2 f as bs
	= all (uncurry f) $ zip as bs


-- groups entries with the same a together
merge		:: Eq a => [(a,b)] -> [(a,[b])]
merge []	= []
merge ((a,b):ls)
		= let bs	= map snd $ filter ((==) a . fst) ls in
			(a,b:bs): merge (filter ((/=) a . fst) ls)
merge'		:: Eq b => [(a,b)] -> [([a],b)]
merge'		= map swap . merge . map swap

unmerge		:: [(a,[b])] -> [(a,b)]
unmerge 	=  concatMap (\(a,bs) -> [(a,b) | b <- bs])

invertDict	:: (Eq b, Ord b, Ord a) => Map a (Set b) -> Map b (Set a)
invertDict	= fmap Set.fromList . Map.fromList . merge . map swap . unmerge . Map.toList . fmap Set.toList

invertDict'	:: (Ord a, Ord b, Eq b) => Map a b -> Map [b] a
invertDict'	=  Map.fromList . map swap . merge . Map.toList

-- makes sure the string always takes (at least) 8*t characters
tabs	:: Int -> String -> String
tabs t str
	= str ++ replicate (t - (length str `div` 8)) '\t'


insertLst	:: Ord k => k -> v -> Map k [v] -> Map k [v]
insertLst k v dict
		=  case Map.lookup k dict of
			Nothing		-> insert k [v] dict
			(Just lst)	-> insert k (v:lst) dict

deleteFromLst	:: (Ord k, Eq v) => v -> k -> Map k [v] -> Map k [v]
deleteFromLst n	=  adjust (filter (n /=))

lookupLst	:: Ord k => k -> Map k [v] -> [v]
lookupLst 	=  findWithDefault []


lstrip		:: String -> String
lstrip (' ':str)	= lstrip str
lstrip s	= s

rstrip	= reverse . lstrip . reverse

strip	= lstrip . rstrip
