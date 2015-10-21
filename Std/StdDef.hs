module StdDef where

import Data.Tuple
import Data.Map (Map, insert, adjust, findWithDefault)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Set (Set)
import Data.List (sortBy)
import Data.Function (on)
import qualified Data.Function
import Data.Maybe (mapMaybe)

import System.Directory

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

onFirst		:: Monad m => (a -> m c) -> (a, b) -> m (c, b)
onFirst f (a, b)
		= do	c	<- f a
			return (c, b)

onSecond	:: Monad m => (b -> m c) -> (a, b) -> m (a, c)
onSecond f (a, b)
		= do	c	<- f b
			return (a, c)

unpackFirst	:: Monad m => (m a, b) -> m (a, b)
unpackFirst (ma, b)
		= do	a 	<- ma
			return (a,b)

unpackSecond	:: Monad m => (a, m b) -> m (a, b)
unpackSecond (a, mb)
		= do	b 	<- mb
			return (a,b)

unpackMaybeTuple
		:: (Maybe a, b) -> Maybe (a,b)
unpackMaybeTuple	= unpackFirst

unpackMaybeTuples	:: [(Maybe a, b)] -> [(a,b)]
unpackMaybeTuples	=  mapMaybe unpackMaybeTuple




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

(|||>>>)	:: (Functor f, Functor g, Functor h)	=> f (g (h a)) -> (a -> b) -> f (g (h b))
(|||>>>) fga f
	=  fmap (fmap (fmap f)) fga


(|+>) a f	= a |> f & sequence

uncurry3	:: (a -> b -> c -> d) -> (a,b,c) -> d
uncurry3 f (a,b,c)
		= f a b c

indent	:: String -> String
indent str
	= str >>= (\c -> if c == '\n' then "\n   " else [c])

indentl	:: String -> String
indentl	= indent . (++) "\n"

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

-- executes the monad for each a, as long as the condition is met
whileM		:: (Monad m, Functor m) => (a -> m Bool) -> [a] -> m [a]
whileM _ []	=  return []
whileM f (a:as)	=  do	cont	<- f a
			if cont then whileM f as |> (a:) else return []


-- executes the monad for each a in [a], as long as the condition is met
whileM'		:: (Monad m, Functor m) => (x -> Bool) -> (a -> m x) -> [a] -> m [x]
whileM' _ _ []	= return []
whileM' cond f (a:as)
		= do	cont	<- f a
			if cond cont then whileM' cond f as |> (cont:) else return []

-- executes the given monad until it's result satisfies the condition
whileDo	:: Monad m => (a -> Bool) -> m a -> m a
whileDo cond m
	= do	a	<- m
		if cond a then return a else whileDo cond m


whileDo'	:: Monad m => (a -> Bool) -> (a -> m a) -> a -> m a
whileDo' cond m start
	= do	a	<- m start
		if not $ cond a then return a else whileDo' cond m a

whileChanged	:: Monad m => (a -> m (a, Bool)) -> a -> m (a, Bool)
whileChanged act a
		= do	res <- whileDo' snd (\((a_, onceChanged), _)	->
				do	(a', changed)	<- act a_
					return ((a', changed || onceChanged), changed)) ((a, False), False)
			return $ fst res



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
sortOn f	=  sortBy (compare `on` f)


splitOn'	:: (Eq a) => a -> [a] -> [[a]]
splitOn' a	= splitOn (a==)

splitOn		:: (a -> Bool) -> [a] -> [[a]]
splitOn f []	=  []
splitOn f ls	=  let (h,t)	= break f ls in
			h:splitOn f (drop 1 t)

-- Creates the file on the given path. If the needed directories don't exist, create them
writeFile'	:: FilePath -> String -> IO ()
writeFile' fp contents
	= do	let dirPath	= fp & reverse & break ('/'==) & snd & reverse
		createDirectoryIfMissing True dirPath
		writeFile fp contents

fix	=  Data.Function.fix


dictMapM	:: (Monad m, Functor m, Ord k) => (k -> v -> m w) -> Map k v -> m (Map k w)
dictMapM f dict
	= dict & Map.toList & mapM (\(k,v) -> do	a 	<- f k v
							return (k,a)) |> Map.fromList

-- unlines with extra newline at the start
unlines'	:: [String] -> String
unlines' str	= "\n" ++ unlines str
