module Regex.Normalform (normalize) where


-- This module writes the regexes as a normalized form, thus:
-- no nested seqs, or such as Seq [ Seq ['a']], 
-- Invert is pushed down until it is placed right before a Range of Fixed

import Regex.Def
import Normalizable

instance Normalizable Regex where
	normalize 	= n

--Seq, Or, And
--BetweenTimes, MinTimes

n 				:: Regex -> Regex

n (Seq rs)			=  packWhenBig Seq Empty $ filter (Empty /=) $ concatMap unpackSeq $ map n rs
n (Or rs)			=  packWhenBig Or Fail $ concatMap unpackOr $ map n rs
n (And rs)			=  packWhenBig And Fail $ concatMap unpackOr $ map n rs


n (Invert Empty)		=  Empty
n (Invert Any)			=  Fail
n (Invert Fail)			=  Any

n (Invert (Seq rs))		=  n $ Seq $ map Invert rs
n (Invert (Or rs))		=  n $ And $ map Invert rs
n (Invert (And rs))		=  n $ Or $ map Invert rs
n (Invert (BetweenTimes	i j r)) =  n $ BetweenTimes i j $ Invert r
n (Invert (MinTimes i r))	=  n $ MinTimes i $ Invert r
n (Invert (Invert r))		=  n r
n (Invert r)			=  Invert $ n r

n (MinTimes i (BetweenTimes k l r))
				=  n $ MinTimes (i*k) r
n (BetweenTimes i j (MinTimes k r))
				=  n $ MinTimes (k*i) r

n (MinTimes i (MinTimes j r))	=  n $ MinTimes (i*j) r
n (MinTimes i r)		=  if isMinTimes opt || isBetwTimes opt then n $ MinTimes i opt else MinTimes i opt 
					where opt = n r

n (BetweenTimes i j (BetweenTimes k l r))
				= n $ BetweenTimes (i*k) (j*l) r
n (BetweenTimes i j r)		=  if isMinTimes opt || isBetwTimes opt then n $ BetweenTimes i j opt else BetweenTimes i j opt
					where opt = n r


n regex				=  regex


isMinTimes (MinTimes _ _)	= True
isMinTimes _			= False

isBetwTimes (BetweenTimes{})= True
isBetwTimes _			= False

unpackSeq			:: Regex -> [Regex]
unpackSeq (Seq rs)		=  rs
unpackSeq reg			=  [reg]

unpackOr			:: Regex -> [Regex]
unpackOr (Or rs)		=  rs
unpackOr reg			=  [reg]

packWhenBig		:: ([Regex] -> Regex) -> Regex -> [Regex] -> Regex
packWhenBig _ empty []	=  empty
packWhenBig _ _ [r]	=  r
packWhenBig cons _ rs	=  cons rs
