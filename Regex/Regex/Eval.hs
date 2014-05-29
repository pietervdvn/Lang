module Regex.Eval (match) where

import Regex.Def
import Consumer
import Parser
import ConsumerL

type ParserL e a = ConsumerL Char e a

chain		:: [String -> String] -> String -> String
chain flst seed	=  foldr (\f a -> f a) seed $ reverse flst

match		:: Regex -> ParserL e String
match r		=  do	fl <- m r
			return $ chain fl ""

m		:: Regex -> ParserL e [String -> String]

m Empty			= (>*) $ return [id]
m Fail			= (>*) $ char >> abort

m (Fixed c)		= (>*) $ append $ charEq c
m (Range c1 c2)		= (>*) $ append $ charIf (`elem` [c1..c2])
m (Invert (Fixed c))	= (>*) $ append $ charIf (c/=)
m (Invert (Range c1 c2))= (>*) $ append $ charIf (`notElem` [c1..c2])
m Any			= (>*) $ append char

m (Seq [])		= return [id]
m (Seq (r:rs))		= do 	r1	<- m r
				r2	<- m (Seq rs)
				return $ r1++r2

m (Or rs)		= (??) $ map m rs
m (And rs)		= ConsumerL.all $ map m rs

m (BetweenTimes 0 0 r)	= (>*) $ return [id]
m (BetweenTimes 0 j r)  = (do	r1 <- m r
				r2 <- m $ BetweenTimes 0 (j-1) r
				return $ r1++r2)
			  ? return [id]
m (BetweenTimes i j r)   = do	r1 <- m r
				r2 <- m $ BetweenTimes (i-1) (j-1) r
				return $ r1 ++ r2
m (MinTimes 0 r)	= (do	r1 <- m r
				r2 <- m $ MinTimes 0 r
				return $ r1 ++ r2)
			  ? return [id]
m (MinTimes i r)	= do	r1 <- m r
				r2 <- m $ MinTimes (i-1) r
				return $ r1 ++ r2

append			:: Parser e Char -> Parser e [String -> String]
append prs		=  do	a <- prs
				return [(++[a])]
