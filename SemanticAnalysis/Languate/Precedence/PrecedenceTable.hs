module Languate.Precedence.PrecedenceTable where

{--
This module implements the precedence table, a data structure which keeps track what associativity and precedence operators have.
--}

import StdDef
import Exceptions

import Data.Map hiding (map, foldr, null)
import Prelude hiding (lookup)

import Data.Maybe

import Languate.FQN
import Languate.AST
import Languate.World

import Languate.Precedence.PrecTable2MD


type Operator	= Name
data PrecedenceTable	= PrecedenceTable { maxI::Int, op2i :: Map Operator Int, i2op :: Map Int [Operator], op2precMod :: Map Operator PrecModifier }

instance Show PrecedenceTable where
	show (PrecedenceTable _ op2i i2op mods)
		= precTable2md op2i i2op mods

modeOf	:: Int -> PrecedenceTable -> PrecModifier
modeOf index (PrecedenceTable _ _ i2op mods)
	= fromMaybe PrecLeft $ do	repr	<- lookup index i2op
					lookup (head repr) mods

precedenceOf	:: Expression -> PrecedenceTable -> Int
precedenceOf expr (PrecedenceTable tot op2i _ _)
		= if isOperator expr
			then	let (Operator nm)	= expr in
				findWithDefault (tot+1) nm op2i
			else	tot+2	-- plus 2, because normal expressions have a precedence lower then unknown operators (tot+1)
