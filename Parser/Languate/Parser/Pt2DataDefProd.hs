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

-- Gives (docstrings with respective constructor name, adt-sums, typeReqs)
pt2prod	:: ParseTree -> ([ADTSum], [TypeRequirement], [DocString Name])
pt2prod =  pt2a h t s convert . cleanAll ["nlt"]

convert		:: AST -> ([ADTSum], [TypeRequirement], [DocString Name])
convert (Sum sum reqs)
		=  ([sum], nub reqs, [])
convert (Sums sums reqs docs)
		=  (sums, nub reqs, docs)
convert ast	=  convErr modName ast


data AST	= Sum ADTSum [TypeRequirement] -- [DocString Name]
		| Sums [ADTSum] [TypeRequirement] [DocString Name]
		| Comm Comment
		| BarT
	deriving (Show)


h		:: [(Name, ParseTree -> AST)]
h		=  [("sum", uncurry Sum . pt2sum),("comment",Comm . pt2comment)]

t		:: Name -> String -> AST
t _ "|"		=  BarT
t nm cont	=  tokenErr modName nm cont


s		:: Name -> [AST] -> AST
s _ [Sums sums reqs docs, Comm c]
		= Sums sums reqs $ makeDocsFor c sums docs
s _ [Sum sum reqs, Comm c]
		= Sums [sum] reqs $ makeDocsFor c [sum] []
s "prod" (sum0@(Sum {}):sum1@(Sums {}):Comm c:tail)
		= let 	sum2			= s "prod" tail
			(sums, reqs, oldDocs)	= accSums [sum0, sum1, sum2]
			newDocs			= makeDocsFor c sums oldDocs   in
			Sums sums reqs (newDocs ++ oldDocs)
s "prod" (Sum sum reqs:Comm c:tail)
		= s "prod" (Sum sum reqs:Sums [] [] []:Comm c:tail)
s "commentedSum" []
		= Sums [] [] []
s "commentedSum" (Comm c:BarT:Sum sum reqs:tail)
		= let 	Sums sums reqs' docs' = s "commentedSum" tail
			oldDocs		= docs'
			newDocs		= makeDocsFor c (sum:sums) oldDocs in
			Sums (sum:sums) (reqs ++ reqs') (newDocs ++ oldDocs)
s _ asts@(Sum _ _:_)
		= uncurry3 Sums $ accSums asts
s _ [BarT, ast]	= ast
s _ [ast]	= ast
s nm asts	= seqErr modName nm asts


makeDocsFor	:: Comment -> [ADTSum] -> [DocString Name] -> [DocString Name]
makeDocsFor c sums alreadyHave
		= concatMap (makeDocstringIf (map about alreadyHave) c) sums

makeDocstringIf	:: [Name] -> Comment -> ADTSum -> [DocString Name]
makeDocstringIf alreadyHave c (ADTSum nm _ _)
		= if nm `notElem` alreadyHave then [DocString c nm]
			else []


accSums		:: [AST] -> ([ADTSum], [TypeRequirement], [DocString Name])
accSums []	=  ([],[],[])
accSums (Sum adt req:tail)
		= let (adts, reqs, docs) = accSums tail in
			(adt:adts, req++reqs, docs)
accSums (Sums adts reqs docs:tail)
		= let (adts', reqs', docs') = accSums tail in
			(adts ++ adts', reqs ++ reqs', docs')
accSums asts	= error $ show asts
