module StdDef where

import Data.Tuple
import Data.Map (Map, insert, adjust, findWithDefault)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Set (Set)
import Data.List (sortBy)


type Name 	= String
type Message	= String
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

unpackMaybeTuple
		:: (Maybe a, b) -> Maybe (a,b)
unpackMaybeTuple (ma,b)
		= do	a	<- ma
			return (a,b)


dubbles		:: Eq a => [a] -> [a]
dubbles []	=  []
dubbles (a:as)	=  (if a `elem` as then (a:) else id) $ dubbles as


longest		:: [[a]] -> [a]
longest ass	=  let	long	= maximum $ map length ass in
			head $ filter ((==) long . length) ass




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

invertDict'	:: (Ord a, Ord b, Eq b) => Map a b -> Map b [a]
invertDict'	=  Map.fromList .  merge . map swap . Map.toList

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

stripnl('\n':str)	= stripnl str
stripnl s	= s


(&)	:: a -> (a -> b) -> b
(&) a f	= f a

(|>)	:: (Functor f)	=> f a -> (a -> b) -> f b
(|>)	= flip fmap

(||>>)	:: (Functor f, Functor g)	=> f (g a) -> (a -> b) -> f (g b)
(||>>) fga f
	=  fmap (fmap f) fga

uncurry3	:: (a -> b -> c -> d) -> (a,b,c) -> d
uncurry3 f (a,b,c)
		= f a b c

indent	:: String -> String
indent str
	= str >>= (\c -> if c == '\n' then "\n   " else [c])

indent'	:: String -> String -> String
indent' msg str
	= msg ++ indent ("\n" ++  str)


first3 f (a,b,c)	= (f a, b, c)
second3 f (a,b,c)	= (a, f b, c)
third3 f (a,b,c)	= (a, b, f c)

fst3 (a,b,c)		= a
snd3 (a,b,c)		= b
thd3 (a,b,c)		= c


firstJust (Just a) _	= Just a
firstJust _ a		= a

perms	:: [[a]] -> [[a]]
perms []	= []
perms [ls]	= [[l] | l <- ls]
perms (ls:lss)
	= do	l	<- ls
		map (l:) $ perms lss

-- Returns the first counter for which the condition is *not* met
while		:: (counter -> Bool) -> (counter -> counter) -> counter -> counter
while cond inc counter
 | cond counter	= counter
 | otherwise	= while cond inc (inc counter)

whileM		:: (Monad m, Functor m) => (a -> m Bool) -> [a] -> m [a]
whileM _ []	=  return []
whileM f (a:as)	=  do	cont	<- f a
			if cont then whileM f as |> (a:) else return []


whileM'		:: (Monad m, Functor m) => (x -> Bool) -> (a -> m x) -> [a] -> m [x]
whileM' _ _ []	= return []
whileM' cond f (a:as)
		= do	cont	<- f a
			if cond cont then whileM' cond f as |> (cont:) else return []


mapTuple	:: (a -> b, c -> d) -> (a,c) -> (b,d)
mapTuple (f, g) (a,b)
		= (f a, g b)

fst4		:: (a,b,c,d) -> a
fst4 (a,_,_,_)	=  a

snd4		:: (a,b,c,d) -> b
snd4 (_,b,_,_)	=  b


isLeft		:: Either a b -> Bool
isLeft (Left _)	= True
isLeft _	= False

isRight		= not . isLeft

sortOn		:: Ord b => (a -> b) -> [a] -> [a]
sortOn f	=  sortBy (\a0 a1 -> f a0 `compare` f a1)
