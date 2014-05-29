module EscUtils where

-- escape charToEsc escChar
-- e.g. escape '"' '\\' "abc\"def" == "abc\\\"def"
-- escape '+' 'a' "abcdea" == "+abcde+a"
 -- backslash double quote (chars == string)
escape			:: Char -> Char -> String -> String
escape _ _ []		=  []
escape bs dq (c:cs)	
	| dq == c	= bs:c:escape bs dq cs
	| otherwise	= c:escape bs dq cs

stresc		:: String -> String
stresc str	=  "\""++ extraEsc ( escape '\\' '"' str) ++ "\""

extraEsc		:: String -> String
extraEsc		=  concatMap specCharsEsc

specCharsEsc		:: Char -> String
specCharsEsc '\n'	=  "\\n"
specCharsEsc '\r'	=  "\\r"
specCharsEsc '\t'	=  "\\t"
specCharsEsc a		=  [a]


-- unescape charToEsc escChar 
-- unescape '"' '\\' 'abc\\\"def" == "abc\'def"
-- unescape '+' 'a' '+abcde+a" == "abcdea"
-- unescape $ escape a b str	== str
unescape		:: Char -> Char -> String -> String
unescape _ _ []		=  []
unescape _ _ [a]	=  [a]
unescape bs dq (b:q:cs)
	| b == bs && q == dq	= q:unescape bs dq cs
	| otherwise		= b:unescape bs dq (q:cs)

-- produces 1+(i/8) "\t" chars"
fill		:: Int -> String
fill i		=  replicate (i `div` 8) '\t'

-- takes the desired length and the length of a string, will return the number of tabs needed
fillL		:: Int -> Int -> Int
fillL ll strl 	=  ll - (strl `div` 8)*8

-- takes a desired alignment (in tabs) and a string, returns this string with the needed tabs added 
addFill		:: Int -> String -> String
addFill l str	=  str ++ fill (fillL (l*8) (-2 + length str))

spaces		:: Int -> String
spaces		=  flip replicate ' '

fixedLen	:: Int -> String -> String
fixedLen i s	=  spaces (i - length s) ++ s

