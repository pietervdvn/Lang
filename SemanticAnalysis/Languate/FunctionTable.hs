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
	{ defined	:: Map Name ([RType],[RTypeReq])	-- signatures of locally defined functions, which might be private
	, public	:: Map Name ([RType],[RTypeReq])	-- all public functions
	}
	deriving (Show)

type FunctionTables	= Map FQN FunctionTable

funcSign2md	:: (Name, ([RType],[RTypeReq])) -> [MarkDown]
funcSign2md (nm, (tps, reqs))	=
	[ bold nm, tps |> st True |> code & unwords, rTypeReqs2md reqs]

functiontable2md	:: FunctionTable -> MarkDown
functiontable2md ft	=
	let header	= ["Name","Types","Requirements"]
	    contents f	= f ft & toList |> funcSign2md
	    tableFor f	= table header $ contents f
		in
		title 2 "Defined" ++ tableFor defined ++
		title 2 "Exported" ++ tableFor public

functiontables2md	:: FunctionTables -> MarkDown
functiontables2md fts	=  fts & toList ||>> functiontable2md
				|> (\(fqn, md) -> title 1 (show fqn) ++ md) & unlines
