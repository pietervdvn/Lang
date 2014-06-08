module Regex.Def (RegexDesc, RegexExc, IRegexExc,
	Regex( Any, Empty, Fail,
			Range, Fixed, 
			Seq, Or, And, Invert,
			BetweenTimes, MinTimes),
	startsWithWS
		) where

import Consumer
type IRegexExc	= Exception Char () -- Internal Regex Exception, when passed outside of this module, repacked into a RegexExc where the entire RegexDesc (the failing regex) is passed
type RegexExc	= Exception Char RegexDesc
type RegexDesc	= String

-- Empty Fail Any Fixed Range Invert Seq Or And BetweenTimes MinTimes
data Regex =      Empty				-- does not consume a char, used for technical reasons. Represented as "" (empty string). Always matches. Invert Empty == Empty
		
		| Fail				-- Fail: Consumes the next char, and fails on it. Invert Fail = Any
		-- atomic, consumes one char
		| Any 				-- any char, represented as "."; Invert Any == fail	
		| Fixed Char 			-- Fixed char: "a", will match one char 'a'; tNot "a" will match any char, except 'a'
		| Range Char Char 		-- Range: "[a..z]": matches one char that is between 'a' and 'z' in the ascii-table. [a..z]! will match any char <'a' and >'z'
		| Invert Regex 			-- Invert: inverts the affected chars: a! becomes all except a, (a|b|c)! == [a..c]! == all except these
		
		| Seq [Regex] 			-- Sequence: the list will be matched sequentially one after another

		| Or [Regex]			-- Or: Choose exactly one option. Or [] == Fail, Or [reg] = reg
		| And [Regex] 			-- And: Try all the options. Continue if all succeed, on all tracks. And [] == Empty. Can be optimized out.

		| BetweenTimes Int Int Regex	--  BetweenTimes: "{i,j}". i <= j
		| MinTimes Int Regex		-- At least i times
	deriving (Eq, Ord)


-- checks if this regex might match whitespace as first chars; assumes normalized input
startsWithWS	:: Regex -> Bool
startsWithWS (Fixed c)
		= c `elem` " \t"
startsWithWS (Range c1 c2)
		= inRange c1 c2 ' ' || inRange c1 c2 '\t'
startsWithWS (Invert rgx)
		= not $ startsWithWS rgx
startsWithWS (Seq (rgx:_))
		= startsWithWS rgx
startsWithWS (Or rgxes)
		= or $ map startsWithWS rgxes
startsWithWS (And rgxes)
		= or $ map startsWithWS rgxes	-- yes, we do or; if one of the firsts is sensitive all of them are
startsWithWS (BetweenTimes _ _ rgx)
		= startsWithWS rgx
startsWithWS (MinTimes _ rgx)
		= startsWithWS rgx
startsWithWS _	= False

inRange		:: Ord a => a -> a -> a -> Bool
inRange a1 a2 a
		= a1 <= a && a2 >= a
