module Languate.RightLeaningTree where

{--

This module implements a right leaning tree. When a thing is added, an index is needed.

> empty `add` ('a',0) `add` ('b',1) `add` ('c',0)
->

  b
 / \
a   c


--}


data RLT a	= Node Int [a] [RLT a]
		| Nil
	deriving (Show)


build		:: [(a,Int)] -> RLT a
build []	=  Nil
build as	=  let grouped	= group as in
			case grouped of
				[group]		-> singleton group
				(group:groups)	-> foldl (\acc a -> add acc a) (singleton $ group) groups


flatten		:: ([a] -> [b] -> b) -> RLT a -> b
flatten f (Node _ as trees)
		=  f as $ map (flatten f) $ filter (not . isNil) trees



-- flattens in haskell function syntax
funcFlatten	= flatten (\as bs -> (concatMap (\c -> ' ':[c]) as) ++ "(" ++ unwords bs ++ ")")

isNil Nil	= True
isNil _		= False


add		:: RLT a -> ([a],Int) -> RLT a
add Nil as	=  singleton as
add node@(Node j cont rlts) t@(as, i)
	| i > j	= Node i as [node]
	| i == j	= error "Not grouped"
	| i < j	= Node j cont (rlts++ [singleton t])

singleton	:: ([a], Int) -> RLT a
singleton (as, i)
		= Node i as [Nil]


group		:: [(a,Int)] -> [([a],Int)]
group []	=  [	]
group ((a,i):as)	= let (sames,rest)	= same i as in
				(a:sames, i):group rest


same		:: Int -> [(a,Int)] -> ([a], [(a,Int)])
same _ []	= ([], [])
same i ls@((a,j):as)
	| i == j	= let (as', rest)	= same i as in
				(a:as', rest)
	| otherwise	= ([], ls)

lowest		:: [Int] -> Int
lowest [i]	=  i
lowest (i:is)	=  let j	= lowest is in
			if i < j then i else j
