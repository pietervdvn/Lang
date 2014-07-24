module Languate.TypeChecker where

import Languate.AST
import Languate.SymbolTable
import Languate.BuiltIns
import StdDef
import Data.Map (Map, insert, member, keys)
import qualified Data.Map as M
import Data.Maybe
import Languate.Order
import Control.Monad.Reader
import Normalizable
import Languate.TypeBinding

data TypedExpression	= TNat Int	| TFlt Float	| TChr Char	-- primitives
			{- the first argument, [Type] are all the possible **return** types. E.g. '(&&) True False' -> Call [Bool] "&&" [..., ...]; '(&&) True' -> Call [Bool -> Bool] -}
			| TApplication [Type] TypedExpression [TypedExpression]
			| TCall [Type] Name	
	deriving (Show)
			

-- rightmost:	a & b & c	=> a & (b & c)	(rightmost, shortest expression get's evaluated first)
-- leftmost:    a & b & c	=> (a & b) & c
-- prefix	a & b & c	=> a (& b (& c))
data InfixMode	= RightMost	| LeftMost	| Prefix	
data Context	= Context {typeTable::TypeTable, priorityTable::PriorityTable}
	deriving (Show)

-- the context gives the type of each 
typeCheck	:: Expression -> Reader Context TypedExpression
typeCheck (Nat i)
		= return $ TNat i
typeCheck (Flt f)
		= return $ TFlt f
typeCheck (Chr c)
		= return $ TChr c
typeCheck (Seq exprs)
		= do	ctx	<- ask
			let call	= runReader (asCall $ Seq exprs) $ priorityTable ctx
			checkCall call
typeCheck (Tuple exprs)
		= do	ctx		<- ask
			typed		<- mapM typeCheck exprs
			let types	= map typeOf typed
		 	let possible	= combine types
		  	return $ TApplication (map (Applied $ Normal "Tuple") possible) (TCall [] "#asTuple") typed	
typeCheck (BuiltIn name)
		= do	let typ	= getBuiltinType name
			return $ TCall [typ] ('#':name)
-- TODO: casts
typeCheck (Cast t)
		= todos "typecheck: Casts: search path in typechecker"
typeCheck AutoCast
		= todos "typehcheck: Autocast"
typeCheck (Operator op)
		= do	ctx	<- ask
			return $ callFor ctx op
typeCheck (Call name)
		= do	ctx	<- ask
			return $ callFor ctx name
typeCheck (ExpNl _)
		= error "typecheck encountered a nl, your expression was not cleaned"


-- simple function call; operator/function without args
callFor	ctx nm	= TCall (lookupType (typeTable ctx) nm) nm



-- application: an expression as (+1) is applied on arguments; e.g. (+1) 1
checkCall	:: Call -> Reader Context TypedExpression
checkCall (Expr _ _ e)
		= typeCheck e
checkCall (FCall _ _ function args)
		= do	typedArgs	<- mapM checkCall args
			typedFunction	<- checkCall function
			let types	= apply (typeOf typedFunction) $ combine $ map typeOf typedArgs
			return $ TApplication types typedFunction typedArgs

-- tries to apply/reduce the type, filters out non-matching combinations
-- > apply [ Nat -> Nat -> Nat] [Nat] = [Nat -> Nat]
-- > apply [ Bool -> Nat -> Nat, Nat -> Nat -> Nat] [Bool] = [Nat -> Nat]
-- > apply [ Nat -> Nat] [Bool] = []
apply'		:: [Type] -> [Type] -> [Type]
apply' funcTypes []	= funcTypes
apply' functTypes (argType:argTypes)
		=  do	typ	<- fmap normalize functTypes
			case typ of
				Curry (t:ts)	-> if argType `fitsIn` t then apply' [Curry $ bind argType t ts] (bind argType t argTypes) else []
				t		-> []



apply		:: [Type] -> [[Type]] -> [Type]
apply funcTypes argTypess
		= do	argTypes	<- argTypess
			apply' funcTypes argTypes


typeOf		:: TypedExpression -> [Type]
typeOf (TNat _)	=  [Normal "Nat", Normal "Int"]
typeOf (TFlt _)
		=  [Normal "Float"]
typeOf (TChr _)	=  [Normal "Char"]
typeOf (TCall tps _)
		=  tps
typeOf (TApplication tps _ _)
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
