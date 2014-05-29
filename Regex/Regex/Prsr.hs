module Regex.Prsr (regexParser) where

-- This module parses string to regexes according as the regex.bnf file

import Regex.Def
import Regex.MetaChars

import Consumer
import Parser

type SParser a	= Parser IRegexExc a

-- notOrEsced A	= escape A | !A
notOrEsced	:: String -> Parser e Char
notOrEsced nt	=  (do	charEq '\\'
			charIf (`elem` nt))
		   >: charIf (`notElem` ('\\':nt))

-- : escape 	::= "\\'
escape		=  charEq '\\'
-- : metachar	::= escape | "\(" | "\)" | "[" | "]" | "\{" | "\}" | "\+" | "\*" | "\?" | "\|" | "." | "#"
metachar	:: Parser e Regex
metachar	= (>:>) $ map (\ (c,r) -> (stringEq c >> return r) ) simpleMetaChars



-- : normalChar ::= escape metachar | ! metachar
normalChar	:: Parser e Regex
normalChar	=  do 	c <- notOrEsced metachars
			return $ Fixed c
 			
-- : range 	::= "[" rangeUnit+ "]"
range		:: SParser Regex
range		=  pair "[" "]" (do 	all <- while _rangeUnit
					return $ Or all) 

-- parses a single character that is valid inside a range [ ]
inRangeUnChar	:: SParser Char
inRangeUnChar	= notOrEsced ".]\\"
		  >:(do	charEq '\\'
			c <- charIf (`elem` "tnf[")
			return $ case c of
				't' -> '\t'
				'r' -> '\r'
				'n' -> '\n'
				'[' -> '[')
		 	
		

-- : _ ruc		:: escape "\]" | escape "\." | ! "\]\."
-- : _ rangeUnit	::= (ruc "\.\." ruc) | ruc
_rangeUnit	:: SParser Regex
_rangeUnit	= (do	st <- state
			c1 <- inRangeUnChar
			stringEq ".."
			c2 <- inRangeUnChar
			throwIf (c2 < c1) $ rangeExc c1 c2 st
			return $ Range c1 c2) 
		>: ( do c <- inRangeUnChar
			return $ Fixed c)


rangeExc		:: Char -> Char -> State Char -> IRegexExc
rangeExc c1 c2	state 	=  Exception state ("in the range '["++[c1]++".."++[c2]++"]'; the char before '..' should be smaller then the other; expected: '["++[c2]++".."++[c1]++"]'") ()


-- : pass1	::= (normalChar | range | metachar | "(" regex ")" ) +
pass1		:: SParser Regex
pass1		=  range >: metachar >: normalChar >: pair "(" ")" regexParser

-- : pass2	::= "\!"? pass1
pass2		:: SParser Regex
pass2		=  (do	charEq '!'
			r <- pass1
			return $ Invert r	
			)>:pass1

-- : int	::= "[0..9]*"
-- postfix	::= "*" | "+" | "?"
postFixes	:: [ (Char, Regex -> Regex) ]
postFixes	=  [ ('*', MinTimes 0), ('+', MinTimes 1), ('?', BetweenTimes 0 1) ]

postfix		:: Parser e (Regex -> Regex)
postfix		=  (>:>) $ map (\(c, f) -> (charEq c >> return f)) postFixes

-- : times		::= "\{" ( int | int? "," int?) "\}"

times		:: SParser (Regex -> Regex)
times		=  pair "{" "}" (
			(do	st <- state
				i <- int
				charEq ','
				j <- int
				throwIf (i > j) $ timesExc i j st
				return (BetweenTimes i j)
			)>:(do	i <- int
				charEq ','
				return (MinTimes i)
			)>:(do	charEq ','
				j <- int
				return (BetweenTimes 0 j)
			)>:(do	charEq ','
				return (MinTimes 0)
			)>:(do 	i <- int
				return (BetweenTimes i i)
			)
		)

timesExc		:: Int -> Int -> State Char -> IRegexExc
timesExc i j state 	=  Exception state ("in the quantity '{"++show i++".."++show j++"}'; the first int should be smaller then the second; expected: '{"++show j++","++show i++"}'") ()


-- : pass3	::= pass2 (postfix | times)?
pass3		:: SParser Regex
pass3		= do r <- pass2
		     (do	fr <- postfix; return $ fr r)
			>: (do	fr <- times; return $ fr r)
			>: return r


-- pass4 takes a as long as possible pass3 (seqed together)
pass4		:: SParser Regex
pass4		=  do 	all <- while pass3
			return $ Seq all
					
-- pass5 will try to fit multiple passes 4 into one OR/AND structure
pass5		:: SParser Regex
pass5		=  do	reg <- pass4
			(do	regs <- while (charEq '|' >> pass4)
				return $ Or (reg:regs)
			 )>:(do	regs <- while (charEq '&' >> pass4)
			 	return $ And (reg:regs)
			 )>: return reg
			
regexParser	:: SParser Regex
regexParser	=  do	regs <- while pass5
			return $ Seq regs

