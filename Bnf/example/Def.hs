module Def where

{--

This module implements the data structures needed for the straight-line program

--}

import StdDef	-- needed for Name

data Expr	= Integer Int
		| Call Name -- Name is another name for String
		| Add Expr Expr
	deriving (Show)

data Statement  = PrintStmt Expr
        | AssignStmt Name Expr
	deriving (Show)
