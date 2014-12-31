module Bnf.ParseTree where

import Regex
import StdDef
import Bnf.FQN
import Data.Maybe
import Data.List (intersperse)
import Normalizable
import Control.Monad.Writer hiding (getFirst, getLast)
{--

This module implements the basic ADT that represents a parse tree.
--}

type Line 	= Int
type Column 	= Int

-- a Coor represents info about the position (char in stream), line nr (how many lines have been passed) and column (the char line)
type Coor 	= (Pos, Line, Column)

-- info about a node: what name it has, and what file it has been defined in
type NodeInfo	= (FQN, Name, Coor)

data ParseTree	= T NodeInfo String
		| S NodeInfo [ParseTree]


getContent	:: ParseTree -> String
getContent (T _ str)	= str
getContent (S _ pts)	= concatMap getContent pts

-- length $ getContent pt === getLength pt
getLength		:: ParseTree -> Int
getLength (T _ str)	= length str
getLength (S _ pts)	= foldl (\ acc pt -> acc + getLength pt) 0 pts

-- parseLength >= length, actually parsed chars
getParseLength		:: ParseTree -> Int
getParseLength pt	=  let 	(pos,_,_) = getEndCoor pt in
				pos

-- gives the coordinate of the last parsed char
getEndCoor		:: ParseTree -> Coor
getEndCoor (T (_,_,(p,l,c)) str)
			= (p + length str, l, c + length str)
getEndCoor seq		= getEndCoor $ getLast seq

instance Normalizable ParseTree where
	normalize	= n

n	:: ParseTree -> ParseTree
n t@(T{})	= t
n (S i rs)	= S i $ filter (not . _isEmpty) $ map normalize rs

getName		:: NodeInfo -> Name
getName (_,name,_)	= name

-- gets the position of the leftmost token
getPosition	:: ParseTree -> Position
getPosition pt	=  let (_,_,(_,l,c))	= getInf $ getFirst pt in
			(l,c)

getInf		:: ParseTree -> NodeInfo
getInf (T inf _)	= inf
getInf (S inf _)	= inf

getFirst	:: ParseTree -> ParseTree
getFirst (S _ (pt:_))	= getFirst pt
getFirst pt	= pt

getLast		:: ParseTree -> ParseTree
getLast (S _ pts)
		= getLast $ last pts
getLast pt	= pt

_isEmpty	:: ParseTree -> Bool
_isEmpty (S _ [])	= True
_isEmpty _		= False

cleanAll	:: [Name] -> ParseTree -> ParseTree
cleanAll ls	= cleanPt (`elem` ls)

cleanPt	:: (Name -> Bool) -> ParseTree -> ParseTree
cleanPt f	= filterPt (not . f)

filterPt	:: (Name -> Bool) -> ParseTree -> ParseTree
filterPt f (S inf subs)
		= S inf $ map (filterPt f) $ filter (f . getName . getInf) subs
filterPt _ token
		= token

instance Show ParseTree where
	show = _s

-- simplification for humans only
simplify	:: ParseTree -> ParseTree
simplify (S _ [pt])
		= simplify pt
simplify (S inf pts)
		= S inf $ map simplify pts
simplify pt	= pt


_s		:: ParseTree -> String
_s (T inf cont)	= "$"++ showInf inf ++ show cont
_s (S inf pts)	= "+"++ showInf inf ++ "\n" ++ add (concatMap (lines . _s) pts)


add		:: [String] -> String
add		=  concatMap (\str -> "  " ++ str ++ "\n")


sind		:: Int -> String
sind		=  flip replicate ' '


fill		:: Int -> String -> String
fill i str	=  replicate (i-length str) ' ' ++ str

showInf		:: NodeInfo -> String
showInf ri@(_,_,coor)	= showRI ri ++" "++ showCoor coor ++":    "


showRI		:: NodeInfo -> String
showRI (fqn, rule, _)
		=  show fqn++"."++rule


showCoor	:: Coor -> String
showCoor (_, l, c)
		= fill 3 (show l)++", c"++ fill 3 (show c)
