module Languate.FunctionTable.Expr2Texpr where

import StdDef
import Exceptions
import Normalizable


import Languate.AST
import Languate.TAST
import Languate.FQN
import Languate.Package
import Languate.TableOverview
import Languate.FunctionTable
import Languate.CheckUtils

import Languate.TypeTable.Bind.Bind
import Languate.TypeTable.Bind.Substitute


import Data.Map (Map, findWithDefault)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

import StateT
import Control.Monad.Trans

{-

Welcome to the lambda core

This is the heart of the typechecker, where expressions get converted into their typed counterpart.

-}

{-
Builds a typed expression from a AST expression. This involves both typechecking and resolving the exact method.

The requirements should contain all used free type variables within the context

-}
expr2texpr	:: Package -> TableOverview -> FQN -> Map Name [RType] -> OperatorFreeExpression -> Exc TExpression
expr2texpr p to fqn reqs e
	=  runstateT (_e2te $ normalize $ preClean e) (Ctx p to fqn reqs) |> fst

data Ctx = Ctx	{ package	:: Package
		, tables	:: TableOverview
		, location	:: FQN	-- the current location where to search
		, reqs		:: Map Name [RType]
		}

type SCtx a	= StateT Ctx (Exceptions String String) a

_e2te		:: Expression -> SCtx TExpression
_e2te (Nat n)	= return $ TNat n
_e2te (Flt f)	= return $ TFlt f
_e2te (Chr c)	= return $ TChr c
_e2te (Call nm)	= do
	fqn		<- get' location
	funcTables	<- get' tables |> functionTables
				|> unpackFTS
	funcTable	<- lift $ M.lookup fqn funcTables ? errMsg fqn
	signs 	<- lift $ M.lookup nm (known funcTable) ? errMsg' fqn nm
	return $ TCall signs
_e2te (Seq (function:args))= do
	tfunction	<- _e2te function
	targs		<- mapM _e2te args
	types	<- calcTypes (typeOf tfunction) (targs |> typeOf)
	return $ TApplication types tfunction targs

_e2te e		=
	lift $ halt $ "Could not type the expression "++show e++"\n\t"++
	case e of
		ExpNl _	-> "A comment/expnl slipped through the clean"
		Operator _
			-> "Seems like the expression didn't go through prefix rewritting (to remove ops)"
		Cast _	-> "Casts are for a next version :p"
		AutoCast-> "Autocasts are for a next verion :p"
		BuiltIn _
			-> "Builtins?"	-- TODO
		Tuple _	-> "Tuples?"	-- TODO


errMsg fqn	= "No function table found for "++show fqn++".\nThis is a bug in the compiler. (A compiler dev probably passed in a wrong FQN into expr2texpr"
errMsg' fqn nm	= "No function with the name "++nm++" found in the module "++show fqn


{- calculates the resulting types that a function has
Note that each

-}
calcTypes	:: [RTypeUnion] -> [[RTypeUnion]] -> SCtx [RTypeUnion]
calcTypes funcT args
		= todo

calcTypeUnion	:: Context -> RTypeUnion -> [RTypeUnion] -> Exc RTypeUnion
calcTypeUnion ctx rtu rtuArgs
	= todo -- TODO PICKUP


calcType	:: Context -> RType -> [RType] -> Exc RType
calcType ctx t0 []
		= return t0
calcType ctx (RCurry t0 rest) (arg:args)
	= do	let tt	= ctx & tables & typeTable
		let rqs	= ctx & reqs
		-- the argument can come from a different context, we thus rename frees
		let arg'	= substitute' (M.keys rqs) arg & fst
		bnd	<- either halt return $ bind tt rqs arg' t0
		let rest'	= substitute bnd rest
		calcType rest' args


preClean	:: Expression -> Expression
preClean (Seq exps)
		= exps & preClean' & Seq
preClean (Tuple exps)
		= exps & preClean' & Tuple
preClean e	= e

preClean'	:: [Expression] -> [Expression]
preClean' exps	= exps & filter (not . isExpNl) |> preClean
