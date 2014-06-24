module Languate.Parser.Pt2Prelude (parseString, parseChar, parseNat, parseFloat) where
{- Converts prelude things to parsed things -}

import StdDef
import Bnf.ParseTree
import Bnf hiding (simpleConvert)
import Languate.Parser.Utils
import Data.Char
import Data.List
import Data.Maybe
import Control.Arrow


-- ## Parsing Strings

parseString	:: ParseTree -> String
parseString 	= exec tStr

tStr		:: Name -> String -> String
tStr "string" (_:str)
	= unescape $ init str

unescape	:: String -> String
unescape ""	= ""
unescape ('\\':c:cs)
		= c : unescape cs
unescape (c:cs)	= c : unescape cs

-- ## Parsing a char

parseChar	:: ParseTree -> Char
parseChar	= (\(Ch c) -> c) . simpleConvert hChar tChar sChar

data AST	= Quote
		| Ch Char
		| Nat Int

hChar 		=  [("nat", Nat . parseNat)]

sChar		:: Name -> [AST] -> AST
sChar "numericChar" [Quote, Nat i, Quote]
		= Ch $ chr i
sChar _ [a]	= a

tChar		:: Name -> String -> AST
tChar "normalChar" ('\'':c:'\'':[])	
		= Ch c
tChar "escapedChar" ('\'':'\\':c:'\'':[])
		= Ch $ fromJust $ lookup c escaped
tChar "numericChar" _
		= Quote 

escaped	= [('n','\n'),('t','\t'),('r','\r'),('\\','\\'),('\'','\'')]

-- ## Parsing the floats

parseFloat	:: ParseTree -> Float
parseFloat	= exec tFloat

tFloat	:: Name -> String -> Float
tFloat "float" = pFloat . prep


pFloat		:: String -> Float
pFloat str	=  let tuple@(_, tailStr)	= second tail $ break (=='.') str in
		   let (head, tail)	= tuple `both` (fromIntegral . flip parseDec 0) in
			head + (tail / 10 ^length tailStr)

-- ## Parsing natural numbers

parseNat	:: ParseTree -> Int
parseNat	=  exec tNat


tNat		:: Name -> String -> Int
tNat "decimal" nr	= parseDec (prep nr) 0
tNat "hex" nr		= parseHex (prep nr) 0
tNat "binary" nr	= parseBin (prep nr) 0
tNat nm cont		= tokenErr "Pt2Prelude" nm cont


prep	:: String -> String
prep ('0':'x':str)	= prep str
prep ('0':'b':str)	= prep str
prep str	= map toLower $ filter (/= '\'') str

parseBin	:: String -> Int -> Int
parseBin "" acc	= acc
parseBin (n:nr)	acc
		= parseBin nr (ord n - 48 + 2 * acc)

parseHex	:: String -> Int -> Int
parseHex "" acc	= acc
parseHex (n:nr) acc
	| n `elem` ['0'..'9']	= parseHex nr (ord n - 48 + 16 * acc)
	| otherwise	= parseHex nr (ord n - (97 - 10) + 16 * acc)

parseDec	:: String -> Int -> Int
parseDec "" acc	= acc
parseDec (n:nr) acc
		= parseDec nr (ord n - 48 + 10 * acc)

both	:: (a,a) -> (a -> b) -> (b,b)
both (a,b) f	= (f a, f b)
