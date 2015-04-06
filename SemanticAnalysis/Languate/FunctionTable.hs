module Languate.FunctionTable where

import StdDef

import Languate.FQN
import Languate.TAST

import Data.Map

import MarkDown

{-

The function table is associated with a single module and keeps track of all known functions.

-}
data FunctionTable	= FunctionTable
	{ defined	:: Map Name [RType]	-- signatures of locally defined functions, which might be private
	}
	deriving (Show)

type FunctionTables	= Map FQN FunctionTable

functiontable2md	:: FunctionTable -> MarkDown
functiontable2md ft	=
	let contents	= defined ft & toList
				|> (\(nm, tps) -> [bold nm, tps |> show |> code & unwords])
	    header	= ["Name","Types"] in
		table header contents

functiontables2md	:: FunctionTables -> MarkDown
functiontables2md fts	=  fts & toList ||>> functiontable2md
				|> (\(fqn, md) -> title 1 (show fqn) ++ md) & unlines
