module Languate.FunctionTable.Expr2Texpr where

import StdDef
import Normalizable


import Languate.AST
import Languate.TAST
import Languate.FQN
import Languate.Package
import Languate.TableOverview
import Languate.FunctionTable
import Languate.CheckUtils

import Data.Map (Map, findWithDefault)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

import State

{-

Welcome to the lambda core

This is the heart of the typechecker, where expressions get converted into their typed counterpart.

-}

expr2texpr	:: Package -> TableOverview -> FQN -> OperatorFreeExpression -> Exc TExpression
expr2texpr p to fqn e
	=  return $ runstate (_e2te $ normalize $ preClean e) (Ctx p to fqn) & fst

data Ctx = Ctx	{ package	:: Package
		, tables	:: TableOverview
		, location	:: FQN	-- the current location where to search
		}

type SCtx a	= State Ctx a

_e2te		:: Expression -> SCtx TExpression
_e2te (Nat n)	= return $ TNat n 
_e2te (Flt f)	= return $ TFlt f
_e2te (Chr c)	= return $ TChr c
_e2te (Call nm)	= do
	fqn		<- get' location
	funcTable	<- get' tables |> functionTables
				|> unpackFTS 
				|> findWithDefault (error $ errMsg fqn) fqn
	let signs 	= findWithDefault (error $ errMsg' fqn nm) nm $ known funcTable
	todos $ show signs
_e2te (Seq (function:args))= do
	tfunction	<- _e2te function
	targs		<- mapM _e2te args
	todo	-- TODO
_e2te e		= error $ "Could not type the expression "++show e


errMsg fqn	= "No function table found for "++show fqn++".\nThis is a bug in the compiler. (A compiler dev probably passed in a wrong FQN into expr2texpr"
errMsg' fqn nm	= "No function with the name "++nm++" found in the module "++show fqn
preClean	:: Expression -> Expression
preClean (Seq exps)
		= exps & preClean' & Seq
preClean (Tuple exps)
		= exps & preClean' & Tuple
preClean e	= e

preClean'	:: [Expression] -> [Expression]
preClean' exps	= exps & filter (not . isExpNl) |> preClean

