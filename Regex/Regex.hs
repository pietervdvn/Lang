module Regex (	RegexDesc, RegexExc,
		Regex,
		debug, showReg, regex, regexp, match, escape, startsWithWS,
		allMatches, longestMatch ) where


import Regex.Def
import Regex.Shw -- debug, showreg :: Regex -> String
import Regex.MetaChars -- metachars :: String : list of all metachars
import Regex.Prsr -- regexParser :: Parser Regex
import Regex.Normalform -- normalize :: Regex -> Regex
import Regex.Eval -- match :: Regex -> Parser String

import Consumer
import ConsumerL (runL, longest)
import Prelude hiding (catch)
import Parser

import EscUtils
import Data.Maybe

{-
USAGE:

import Consumer
import ConsumerL

start (longest $ match $ regex "<your regex>") "<String to match from start>"
-}


instance Show Regex where
	 -- toggle these options if needed, showReg is the default, classic show while debug gives most information about the regex structure
	show	= showReg -- -}
	--show 	= debug -- -}

regex		:: RegexDesc -> Regex
regex str	=  unpack $ run regexp (str,'\n', 0, (0,0))

longestMatch	:: Regex -> String -> Maybe String
longestMatch rgx str
		=  unpackMaybe $ run (longest $ match rgx) (str,'\n',0,(0,0))

allMatches	:: Regex -> String -> [String]
allMatches rgx str
		=  mapMaybe unpackMaybe $ runL (match rgx) (str,'\n',0,(0,0))


regexp		= catch genHandle regexi

regexi		:: Consumer Char RegexExc Regex
regexi		= do	st 	<- state
			dn 	<- done
			throwIf dn $ emptyExc st
			reg 	<- catch (handle st) regexParser >: throw (invalStrt st)
			done 	<- done
			st2 	<- state
			if not done then throw $ invalChr st st2
				else return $ normalize reg


genHandle	:: RegexExc -> Consumer Char RegexExc a
genHandle (Exception state msg desc)
		=  throw $ Exception state ("Error while parsing regex '"++desc++"'"++msg) desc

emptyExc	:: State Char -> RegexExc
emptyExc st@(d,_,_,_)
		=  Exception st "; no regex given, empty string" d

invalStrt	:: State Char -> RegexExc
invalStrt st@(d,_,_,_)
		=  Exception st "; invalid start of regex" d

handle		:: State Char-> IRegexExc -> Consumer Char RegexExc a
handle (d,_,_,_) (Exception st@(s,i,_,_) msg ())
		=  throw $ Exception st (" at char "++show i++" "++msg) d

invalChr		:: State Char -> State Char -> RegexExc
invalChr (d, _, _, _) st@(s,_, i,_)
			= Exception st (" at char "++show i++"; could not parse '"++take 5 s++"'") d
