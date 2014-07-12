module Languate.TypeChecker where

import Languate.AST
import Languate.SymbolTable
import Languate.BuiltIns
import Languate.RightLeaningTree
import StdDef
import Data.Map (Map, insert, member, keys)
import qualified Data.Map as M
import Data.Maybe


data TypedExpression	= TNat Int	| TFlt Float	| TChr Char	-- primitives
			{- the first argument, [Type] are all the possible **return** types. E.g. '(&&) True False' -> Call [Bool] "&&" [..., ...]; '(&&) True' -> Call [Bool -> Bool] -}
			| TCall [Type] Name [TypedExpression]		
	deriving (Show)
			

-- rightmost:	a & b & c	=> a & (b & c)	(rightmost, shortest expression get's evaluated first)
-- leftmost:    a & b & c	=> (a & b) & c
-- prefix	a & b & c	=> a (& b (& c))
data InfixMode	= RightMost	| LeftMost	| Prefix	
data Context	= Context {typeTable::TypeTable, infixOrdering:: Map Name Int}
	deriving (Show)

-- the context gives the type of each 
typeCheck	:: Context -> Expression -> TypedExpression
typeCheck _ (Nat i)
		= TNat i
typeCheck _ (Flt f)
		= TFlt f
typeCheck _ (Chr c)
		= TChr c
typeCheck ctx (Seq expr)
		= order ctx expr
typeCheck ctx (Tuple exprs)
		= let typed	= map (typeCheck ctx) exprs	in
		  let types	= map typeOf typed   		in
		  let possible	= combine types	     		in
		  	TCall (map (Applied $ Normal "Tuple") possible) "#asTuple" typed	
typeCheck ctx (BuiltIn name)
		= let typ	= getBuiltinType name in
			TCall [typ] ('#':name) []
-- TODO: casts
-- TODO: clean expressions
typeCheck ctx (Cast t)
		= todos "typecheck: Casts: search path in typechecker"
typeCheck ctx AutoCast
		= todos "typehcheck: Autocast"
typeCheck ctx (Operator op)
		= callFor ctx op
typeCheck ctx (Call name)
		= callFor ctx name
typeCheck ctx (ExpNl _)
		= error "typecheck encountered a nl, your expression was not cleaned"

callFor	ctx nm	= TCall (lookupType (typeTable ctx) nm) nm []



typeOf		:: TypedExpression -> [Type]
typeOf (TNat _)	=  [Normal "Nat", Normal "Int"]
typeOf (TFlt _)
		=  [Normal "Float"]
typeOf (TChr _)	=  [Normal "Char"]
typeOf (TCall tps _ _)
		=  tps




-- 0: get's evaluated first
-- the higher, the later it will get evaluated
-- e.g. a	= 0
-- $		= 1000
-- .		= 999
-- unkown ops	= 2
-- a.b.c $ g.h.i	=> (a.(b.c)) (g.(h.i))
-- cast/autocast	= 1
-- a.b.c >=< g.h.i $ d.e.f >=< j.k.l	=> (a.(b.c))
getPriority	:: Map Name Int -> Expression -> Int
getPriority _ (Cast _)
		=  1
getPriority _ AutoCast
		=  1
getPriority ctx (Operator op)
		= fromMaybe 2 $ M.lookup op ctx
getPriority _ _	=  0



-- looks up the type of the given function into the context
-- might crash on a 'not found'
lookupType	:: TypeTable -> Name -> [Type]
lookupType tt name
		= fromMaybe (error $ errMsg name) $ M.lookup name $ cont tt

errMsg name	= "Typechecker: Name not found: "++name++". The pre-typecheck sanitizer was not run (and should be implemented)"


-- generates all possible choices
-- > combine [['a','b'] , ['c','d'] ]	--> ["ac","ad","bc","bd"]
combine :: [[a]] -> [[a]]
combine []	=  [[]]
combine (ts:tss)	
		=  do	t	<- ts
			tail	<- combine tss
			[t:tail]
