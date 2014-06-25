module Languate.Parser.Pt2DataDefProd (pt2prod) where

import StdDef
import Bnf.ParseTree
import Bnf
import Languate.Parser.Utils
import Languate.Parser.Pt2Comment
import Languate.Parser.Pt2DataDefSum
import Languate.AST

{--

This module converts the ParseTree into a ADT-def, eg

data ADT = A a b c
	| D e f

It uses pt2datadefsum to do part of the lifting, the heave work is done by pt2prod (product) which adds the comments to it.
--}

modName	= "Pt2DataDefProd"

pt2prod	:: ParseTree -> [ADTSum]
pt2prod =  pt2a h t s convert . cleanAll ["nlt"]

convert		:: AST -> [ADTSum]
convert (Sum sum)
		=  [sum]
convert (Sums sums)
		=  sums
convert ast	=  convErr modName ast


data AST	= Sum ADTSum
		| Sums [ADTSum]
		| Comm Comment
		| BarT
	deriving (Show)


h		:: [(Name, ParseTree -> AST)]
h		=  [("sum", Sum . pt2sum),("comment",Comm . pt2comment)]

t		:: Name -> String -> AST
t _ "|"		=  BarT
t nm cont	=  tokenErr modName nm cont


s		:: Name -> [AST] -> AST
s _ [Sums sums, Comm c]
		= Sums $ map (setComment c) sums
s _ [Sum sum, Comm c]
		= Sums $ (:[]) $ setComment c sum
s "prod" (Sum sum:Sums sums:Comm c:tail)
		= let Sums sums' = s "prod" tail in
			Sums $ map (setComment c) (sum:sums) ++ sums'
s "prod" (Sum sum:Comm c:tail)
		= s "prod" (Sum sum:Sums []:Comm c:tail)
s "commentedSum" []
		= Sums []
s "commentedSum" (Comm c:BarT:Sum sum:tail)
		= let Sums sums = s "commentedSum" tail in
			Sums $ map (setCommentIf c) $ sum:sums
s _ asts@(Sum _:_)
		= Sums $ accSums asts
s _ [BarT, ast]	= ast
s _ [ast]	= ast
s nm asts	= seqErr modName nm asts

accSums		:: [AST] -> [ADTSum]
accSums []	=  []
accSums (Sum adt:tail)
		= adt: accSums tail
accSums (Sums adts:tail)
		= adts ++ accSums tail
accSums asts	= error $ show asts

