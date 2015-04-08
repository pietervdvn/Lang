module Languate.FunctionTable where

import StdDef

import Languate.FQN
import Languate.TAST

import Data.Set
import Data.Map (Map)
import qualified Data.Set as S
import qualified Data.Map as M

import Languate.MarkUp

{-

The function table is associated with a single module and keeps track of all known functions.

-}
data FunctionTable	= FunctionTable
	{ defined	:: Set Signature-- signatures of locally defined functions, which might be private
	, public	:: Set Signature --all public functions
	}
	deriving (Show)

type FunctionTables	= Map FQN FunctionTable

funcSign2md	:: Signature -> [MarkDown]
funcSign2md sign	=
	[ bold $ signName sign
	, signTypes sign |> st True |> code & unwords
	, signTypeReqs sign & rTypeReqs2md]

functiontable2md	:: FunctionTable -> MarkDown
functiontable2md ft	=
	let header	= ["Name","Types","Requirements"]
	    contents f	= f ft & toList |> funcSign2md
	    tableFor f	= table header $ contents f
		in
		title 2 "Defined" ++ tableFor defined ++
		title 2 "Exported" ++ tableFor public

functiontables2md	:: FunctionTables -> MarkDown
functiontables2md fts	=  fts & M.toList ||>> functiontable2md
				|> (\(fqn, md) -> title 1 (show fqn) ++ md) & unlines
