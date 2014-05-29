module Parser where

import Data.Char
import Consumer

type Parser e a	= Consumer Char e a


char	 	:: Parser e Char
char		=  next

toStr		:: Parser e Char -> Parser e String
toStr prs	=  do 	c <- prs
			return [c]

string		:: Int -> Parser e String
string		=  for
				


charIf		:: (Char -> Bool) -> Parser e Char
charIf b	=  do	c <- char
			if b c then return c else abort
			
charEq		:: Char -> Parser e Char
charEq c	=  charIf (c==)

digit 	:: Parser e Int			
digit	=  do 	c <- char
		if isDigit c then
			return (ord c - ord '0') else abort


integer	:: Parser e Integer
integer	= do 	i <- int
		return $ fromIntegral i

int		:: Parser e Int
int		=   do 	charEq '-'
			i <- posInt
			return (-i)
		 >: posInt

posInt	:: Parser e Int
posInt	= do	d <- digit
		_int d

_int		:: Int -> Parser e Int
_int acc 	=  do	dig <- digit
			_int (dig + acc*10)
		>: return acc

float		:: Parser e Float
float		=  do	charEq '-'
		   	f <- posFloat
		   	return (-f)
		   >: posFloat
		
posFloat 	:: Parser e Float
posFloat 	=  (do
			pre <- posInt
			charIf (`elem` ".,")
			zeros	<- while $ charEq '0'
			post <- posInt
			return $ _float pre post $ length zeros)
		>: (do 	i <- posInt
			return $ fromIntegral i)
			
_float		:: Int -> Int -> Int -> Float
_float pre post zeros
		=  pre' + post'		
	 	  where pre' 	= fromIntegral pre
			post' 	= post'' / (10 ^ (zeros + length ( show post)))
			post'' 	= fromIntegral post
		
stringEq		:: String -> Parser e String
stringEq		=  _fixedString ""

_fixedString 		:: String -> String -> Parser e String
_fixedString acc (c:cs)	=  do 	_ <- charEq c
				_fixedString (acc++[c]) cs
_fixedString acc []	=  return acc

{- | pair matches the opening string, parses with the intermediate Parser e, and the closing string. e.g. for parens: pair "(" ")" yourParser e|-}
pair			:: String -> String -> Parser e a -> Parser e a
pair open close prs	=  do	stringEq open
				a <- prs
				stringEq close
				return a
