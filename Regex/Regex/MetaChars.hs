module Regex.MetaChars where

import Regex.Def
--Empty Fail Any Fixed Range Invert Seq Or And BetweenTimes MinTimes

metachars = "{}!&|()[]*+?.\\"

simpleMetaChars 	= [(".",Any),("\\n", Fixed '\n'),("\\t", Fixed '\t'), ("\\r",Fixed '\r')]  -- ,('#', Fail)]
simpleMetaRegexes	= map snd simpleMetaChars
