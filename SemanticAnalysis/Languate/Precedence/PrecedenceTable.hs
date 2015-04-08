module Languate.Precedence.PrecedenceTable where

{--
This module implements the precedence table, a data structure which keeps track what associativity and precedence operators have.
--}

import StdDef
import Exceptions

import Data.Map hiding (map, foldr, null)
import Data.Set (member, Set)
import qualified Data.Set as S
import Prelude hiding (lookup)
import Data.List (sort)

import Data.Maybe

import Languate.FQN
import Languate.AST

import Languate.MarkUp as Mu

type Operator	= Name
data PrecedenceTable	= PrecedenceTable 
	{ maxI::Int
	, op2i :: Map Operator Int
	, i2op :: Map Int (Set Operator)
	, op2precMod :: Map Operator PrecModifier }
	deriving (Show)

instance Documentable PrecedenceTable where
	toDocument precT
		= (precedenceOverview, [precedencePerLevel precT, precedencePerOperator precT])

modeOf	:: Int -> PrecedenceTable -> PrecModifier
modeOf index (PrecedenceTable _ _ i2op mods)
	= fromMaybe PrecLeft $ do	repr	<- lookup index i2op
					lookup (S.findMin repr) mods

precedenceOf	:: Expression -> PrecedenceTable -> Int
precedenceOf expr (PrecedenceTable tot op2i _ _)
		= if isOperator expr
			then	let (Operator nm)	= expr in
				findWithDefault tot nm op2i
			else	tot+1	-- plus 1, because normal expressions/function application have a precedence lower then unknown operators (tot+1)






------------------ Documentation generation----------------

precPerLevelTitle	= "Operators per precedence levels"
precPerOpTitle		= "Operators with precedence levels"

precedencePerLevel	:: PrecedenceTable -> Doc
precedencePerLevel precTable
	= let	rows	= i2op precTable & toList & sort |> class2MU (op2precMod precTable)
		mxm	= maxI precTable
		lastRw1	= [show $ 1 + mxm, "Other operators", show PrecLeft]
		lastRw2	= [show $ 2 + mxm, "Function application", show PrecLeft]
		lastRows= [lastRw1, lastRw2] ||>> Base
		perLvl	= table ["Precedence","Operators","Associativity"] (rows++lastRows)
		in  doc precPerLevelTitle "Gives the different precedence levels with each operator in this precedence level" perLvl


precedencePerOperator	:: PrecedenceTable -> Doc
precedencePerOperator precTable
	= let	rows	= op2i precTable & toList |> op2mu (op2precMod precTable)
		perOp	= table ["Operator","Precedence", "Associativity"] rows in
		doc precPerOpTitle "Gives all the operators with their respective precedence level" perOp

precedenceOverview	:: Doc
precedenceOverview	= doc "Precedence Overview" "Operators with their respective precedence" $
				 titling "Precedence Overview" $ Mu.Seq [explanation, Embed precPerLevelTitle, Embed precPerOpTitle]

op2mu	:: Map Name PrecModifier -> (Operator, Int) -> [MarkUp]
op2mu mods (op, i)
	=  [code op, Base $ show i, precOf mods op]

class2MU:: Map Operator PrecModifier -> (Int, Set Operator) -> [MarkUp]
class2MU mods (i, ops')
	=  let ops@(repr:_) = S.toList ops' in
			[Base $ show i, ops |> code & commas', precOf mods repr]

precOf	:: Map Operator PrecModifier -> Name -> MarkUp
precOf mods op
	= Base $ maybe "left (default)" show $ lookup op mods


explanation
	= parag "The higher the operator stands in the table (the lower the number), the more range it will have. The lower it stands, the tighter the operator binds. The lower the operator stands, the earlier it will be evaluated\n\nTo test precedence, invoke ````--p <expression>```` in the interpreter, which converts expression to prefix notation."

