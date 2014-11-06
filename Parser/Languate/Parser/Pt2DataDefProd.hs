module Languate.Parser.Pt2DataDefProd (pt2prod) where

import StdDef
import Bnf.ParseTree
import Bnf
import Languate.Parser.Utils
import Languate.Parser.Pt2Comment
import Languate.Parser.Pt2DataDefSum
import Languate.AST

import Data.List (nub)

{--

This module converts the ParseTree into a ADT-def, eg

data ADT = A a b c
	| D e f

It uses pt2datadefsum to do part of the lifting, the heave work is done by pt2prod (product) which adds the comments to it.
--}

modName	= "Pt2DataDefProd"

pt2prod	:: ParseTree -> ([ADTSum], [TypeRequirement])
pt2prod =  pt2a h t s convert . cleanAll ["nlt"]

convert		:: AST -> ([ADTSum], [TypeRequirement])
convert (Sum sum reqs)
		=  ([sum], nub reqs)
convert (Sums sums reqs)
		=  (sums, nub reqs)
convert ast	=  convErr modName ast


data AST	= Sum ADTSum [TypeRequirement]
		| Sums [ADTSum] [TypeRequirement]
		| Comm Comment
		| BarT
	deriving (Show)


h		:: [(Name, ParseTree -> AST)]
h		=  [("sum", uncurry Sum . pt2sum),("comment",Comm . pt2comment)]

t		:: Name -> String -> AST
t _ "|"		=  BarT
t nm cont	=  tokenErr modName nm cont


s		:: Name -> [AST] -> AST
s _ [Sums sums reqs, Comm c]
		= Sums (map (setComment c) sums) reqs
s _ [Sum sum reqs, Comm c]
		= Sums ((:[]) $ setComment c sum) reqs
s "prod" (Sum sum reqs0:Sums sums reqs1:Comm c:tail)
		= let Sums sums' reqs2 = s "prod" tail in
			Sums (map (setComment c) (sum:sums) ++ sums') (reqs0 ++ reqs1 ++ reqs2)
s "prod" (Sum sum reqs:Comm c:tail)
		= s "prod" (Sum sum reqs:Sums [] []:Comm c:tail)
s "commentedSum" []
		= Sums [] []
s "commentedSum" (Comm c:BarT:Sum sum reqs:tail)
		= let Sums sums reqs0 = s "commentedSum" tail in
			Sums (map (setCommentIf c) $ sum:sums) reqs0
s _ asts@(Sum _ _:_)
		= uncurry Sums $ accSums asts
s _ [BarT, ast]	= ast
s _ [ast]	= ast
s nm asts	= seqErr modName nm asts

accSums		:: [AST] -> ([ADTSum], [TypeRequirement])
accSums []	=  ([],[])
accSums (Sum adt req:tail)
		= let (adts, reqs) = accSums tail in
			(adt:adts, req++reqs)
accSums (Sums adts reqs:tail)
		= let (adts', reqs') = accSums tail in
			(adts ++ adts', reqs ++ reqs')
accSums asts	= error $ show asts
