module Regex.Shw (debug, showReg)
	where

import Regex.Def
import Regex.MetaChars

-- this method is the first called with show
-- it shows the sequence *without* ( ), as these are not needed on first call
-- it passes control flow to showreg
showReg			:: Regex -> String
showReg (Seq (r:rs))	= foldl (++) (showreg r) (map showreg rs)
showReg reg	  	= showreg reg

showreg			:: Regex -> String
showreg	r
	| r `elem` simpleMetaRegexes
			= fst $ head $ filter (\(str, reg) -> r == reg) simpleMetaChars
	| otherwise	= shw r  

	
shw			:: Regex -> String
shw Empty		= ""
shw Any			= "."
shw Fail		= "\\#"

shw (Range c1 c2)	= "[" ++ showreg ( Fixed c1) ++".." ++ showreg ( Fixed c2) ++ "]"
				
shw (Fixed c)		= if c `elem` metachars then '\\':[c]
				else [c]

shw (Seq rs)		= foldl (++) "(" (map showreg rs) ++ ")"

shw (Or [])		= ""
shw (Or [r])		= showreg r
shw (Or (r:rs))		= if all (\t -> isFixed t || isRange t) (r:rs) 
				then foldl (++) "[" (map showSpecial (r:rs)) ++ "]"
				else "("++ foldl (\ a b -> a++"|"++b) (showreg r) (map showreg rs) ++")"
					where					
					showSpecial (Range c1 c2)	=  showreg ( Fixed c1 ) ++ ".." ++ showreg ( Fixed c2 )
					showSpecial r			=  showreg r

shw (And [])		= ""
shw (And [r])		= showreg r
shw (And (r:rs))	= "("++ foldl (\ a b -> a++"&"++b) (showreg r) (map showreg rs) ++")"

shw (Invert r)		= '!': showreg r

shw (BetweenTimes 0 0 r) = ""
shw (BetweenTimes 0 1 r) = showreg r ++ "?"
shw (BetweenTimes 1 1 r) = showreg r
shw (BetweenTimes 0 j r) 
	| j < 0		 = error "Betweentimes: expected i < j"
	| otherwise	 = showreg r ++ "{,"++ show j ++"}"
shw (BetweenTimes i j r)
	| j < i 	 = error "Betweentimes: expected i < j"
	| i == j	 = showreg r ++ "{" ++ show i ++ "}"
	| otherwise	 = showreg r ++ "{" ++ show i ++ "," ++ show j ++ "}"

shw (MinTimes 0 r)	= showreg r ++"*"
shw (MinTimes 1 r)	= showreg r ++ "+"
shw (MinTimes i r)	= showreg r ++ "{" ++ show i ++ ",}"



isFixed				:: Regex -> Bool
isFixed (Fixed _)		=  True
isFixed _			=  False

isRange				:: Regex -> Bool
isRange (Range _ _)		=  True
isRange _			=  False


-- Debug show for regex objects
debug	:: Regex -> String
debug	= d

d Empty 	= "Empty"
d Fail 		= "Fail"
d Any 		= "Any"
d (Fixed c)	= '\'':c:"\'"
d (Range c1 c2) = '\'':c1:'\'':'.':'.':'\'':c2:"\'"
d (Invert r) 	= "!("++ debug r ++ ")"
d (Seq rs) 	= "[" ++ p rs " " "" ++  "]"
d (Or rs) 	= "[" ++ p rs " | " "|" ++ "]"
d (And rs) 	= "[" ++ p rs " & " "&" ++ "]"
d (BetweenTimes i j r) 
		= debug r ++ "{"++ show i ++ ","++ show j ++ "}"
d (MinTimes i r) 
		= debug r ++ "{"++ show i ++ ",}"

p		:: [Regex] -> String -> String ->  String
p [] s e	=  e
p [r] _	_	=  debug r
p (r:rs) s e	=  debug r ++ s ++ p rs s e
