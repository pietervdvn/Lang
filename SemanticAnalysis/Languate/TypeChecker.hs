module Languate.TypeChecker where

import StdDef
import Normalizable

import Languate.TAST
import Languate.AST
import Languate.SymbolTable
import Languate.TypeTable
import Languate.BuiltIns
import Languate.TypeBinding

import Languate.Precedence.Precedence

import qualified Data.Map as M
import Data.Map (Map, insert, member, keys)
import Data.Maybe
import Data.List (nub)
import Control.Monad.Reader

-- the context gives the type of each name. Expressions are supposed to be passed through writeAsPrefix in Precedence
typeCheck	:: Expression -> Reader TypeTable TypedExpression
typeCheck (Nat i)
		= return $ TNat i
typeCheck (Flt f)
		= return $ TFlt f
typeCheck (Chr c)
		= return $ TChr c
typeCheck (Seq (Call call:args))
		= do	args'	<- mapM typeCheck args
			func	<- typeCheck $ Call call
			let types	= apply (typeOf func) $ combine $ map typeOf args'
			return $ TApplication (map normalize $ nub types) func args'
typeCheck (Tuple exprs)
		= do	ctx		<- ask
			typed		<- mapM typeCheck exprs
			let types	= map typeOf typed
		 	let possible	= combine types
		  	return $ TApplication (map (Applied $ Normal "Tuple") possible) (TCall [] "#asTuple") typed
typeCheck (BuiltIn name)
		= do	let typ	= normalize $ getBuiltinType name
			return $ TCall [typ] ('#':name)
-- TODO: casts
typeCheck (Cast t)
		= todos "typecheck: Casts: search path in typechecker"
typeCheck AutoCast
		= todos "typehcheck: Autocast"
typeCheck (Operator op)
		= callFor op
typeCheck (Call name)
		= callFor name
typeCheck (ExpNl _)
		= error "typecheck encountered a nl, your expression was not cleaned (this is a bug, contact the dev!)"


-- converts a name into a TCall
callFor		:: Name -> Reader TypeTable TypedExpression
callFor nm	= do	typeTable	<- ask
			return $ TCall (map normalize $ lookupType typeTable nm) nm


-- tries to apply/reduce the type, filters out non-matching combinations
-- note that the second argument represents the types it is applied to.
-- > apply [ Nat -> Nat -> Nat] [Nat] = [Nat -> Nat]
-- > apply [ Bool -> Nat -> Nat, Nat -> Nat -> Nat] [Bool] = [Nat -> Nat]
-- > apply [ Nat -> Nat] [Bool] = []
apply'		:: [Type] -> [Type] -> [Type]
apply' funcTypes []	= funcTypes
apply' functTypes (argType:argTypes)
		=  do	typ	<- fmap normalize functTypes	-- for each of the possibilities (list monad)
			case typ of
				Curry (t:ts)	-> _apply argType argTypes t ts
				t		-> []

_apply argType argTypes t ts
	= if argType `fitsIn` t 							-- if it fits
		then apply' [Curry $ map (bind' argType t) ts] (map (bind' argType t) argTypes) 	-- I sits!
		else []

-- applies all possible argument types on the funtion type, gives all possible types
-- >> let possibleFunctionTypes = [Curry [Nat,Nat] Nat, Curry [Int,Int] Int]
-- >> let possibleArgTypes = [ [Nat,Nat], [Nat,Int], [Int,Nat], [Int,Int]]
-- >> apply possibleFunctionTypes possibleArgTypes = [Nat, Int]
apply		:: [Type] -> [[Type]] -> [Type]
apply funcTypes argTypess
		= do	argTypes	<- argTypess
			apply' funcTypes argTypes




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
		= fromMaybe (error $ errMsg name) $ findType name tt

errMsg name	= "Typechecker: Name not found: "++name++". The pre-typecheck sanitizer was not run; this is a bug"


-- generates all possible choices
-- > combine [['a','b'] , ['c','d'] ]	--> ["ac","ad","bc","bd"]
combine :: [[a]] -> [[a]]
combine []	=  [[]]
combine (ts:tss)
		=  do	t	<- ts
			tail	<- combine tss
			[t:tail]
