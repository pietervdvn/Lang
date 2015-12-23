module Languate.TAST.Defaults where

{--
This module implements functions with some defaults, as defaultFreeNames etc...
--}


import StdDef

import Data.Map

defaultFreeNames	:: [Name]
defaultFreeNames	= defaultFreeNames' "a"


defaultFreeNames'	:: String -> [Name]
defaultFreeNames' nm	= [0..] |> show |> (nm++)


buildMapping	:: Eq b => [(a, b)] -> [(b, c)] -> [(a,c)]
buildMapping start end
	= do	(a,b)	<- start
		(b',c)	<- end
		[(a,c) | b == b']
