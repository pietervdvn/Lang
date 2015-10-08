module Languate.Precedence.PrecedenceTableDef where

{--
This module implements the precedence table, a data structure which keeps track what associativity and precedence operators have.
--}

import Prelude hiding (lookup)
import StdDef
import Exceptions

import Data.Maybe
import Data.Map hiding (map, foldr, null)
import qualified Data.Set as S
import Data.Set (member, Set)

import Languate.FQN
import Languate.AST


type Operator	= Name
data PrecedenceTable	= PrecedenceTable
	{ maxI::Int
	, op2i :: Map Operator Int
	, i2op :: Map Int (Set Operator)
	, op2precMod :: Map Operator PrecModifier
	, rules	:: [([(Name, PrecModifier)], [PrecRelation], FQN)] }
	deriving (Show)



modeOf	:: Int -> PrecedenceTable -> PrecModifier
modeOf index (PrecedenceTable _ _ i2op mods _)
	= fromMaybe PrecLeft $ do	repr	<- lookup index i2op
					lookup (S.findMin repr) mods

precedenceOf	:: Expression -> PrecedenceTable -> Int
precedenceOf expr (PrecedenceTable tot op2i _ _ _)
		= if isOperator expr
			then	let (Operator nm)	= expr in
				findWithDefault tot nm op2i
			else	tot+1	-- plus 1, because normal expressions/function application have a precedence lower then unknown operators (tot+1)
