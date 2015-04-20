module Languate.FunctionTable.Expr2Texpr where

import StdDef
import Exceptions
import Normalizable
import HumanUtils hiding (when)


import Languate.AST
import Languate.TAST
import Languate.FQN
import Languate.Package
import Languate.TableOverview
import Languate.FunctionTable
import Languate.CheckUtils

import Languate.TypeTable hiding (reqs)

import Languate.TypeTable.Bind.Bind
import Languate.TypeTable.Bind.Substitute


import Data.Map (Map, findWithDefault)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.List (nub)
import Data.Either
import Data.Tuple


import StateT
import Control.Monad
import Control.Monad.Trans
import Control.Arrow

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
	-- TODO PICKUP
	-- types	<- calcTypes (typeOf tfunction) (targs |> typeOf)
	--return $ TApplication types tfunction targs
	todo
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


{- calculates the resulting types that a function has.

There is room for an extra parameter, (e.g. the signature) which will be passed.

-}
calcTypes	:: [(RTypeUnion, a)] -> [(RTypeUnion,a)] ->
			SCtx [((a,a), RTypeUnion)]
calcTypes func arg
	= do	let tps	= [ (((funcT, argT),(funcA,argA)), (funcT, argT)) | (funcT, funcA) <- func, (argT, argA) <- arg]
		ctx	<- get
		let res	= tps ||>> uncurry (calcTypeUnion ctx) ||>> runExceptions ||>> thd3 -- :: [((([RType], [RType]), (a,a)), Either String RTypeUnion)]
		let smsg (((funcUnion, argUnion), _), Left msg)
			= "The typeunion "++ (funcUnion |> st True & unwords)++" could not be applied on the types "++(argUnion |> st True & unwords)++":\n"++indent msg
		let msg	= res |> smsg & unlines	:: String
		let ok	= res	|> (first snd)
				& filter (isRight . snd) ||>> (\(Right t) -> t)
		when (null ok) $ lift $ halt $ "Not a single type union gave a result:\n"++indent msg
		return ok

{-
A function can have multiple typeunions, e.g.

(+)	: Int -> Int -> Int, Associative Int
Applied on
1	: Int, Nat

Results in
[Int -> Int]

Note that the associative dissappears

-}
calcTypeUnion	:: Ctx -> RTypeUnion -> RTypeUnion -> Exc RTypeUnion
calcTypeUnion ctx rtu rtuArg
	= do	let tps = [(baseType, argType) | baseType <- rtu, argType <- rtuArg]
				|> (id &&& (uncurry $ calcType ctx))
				||>> runExceptions ||>> thd3
		let rst	= tps |> snd & rights	:: RTypeUnion
		let msg	= tps |> (\((base, arg), Left msg) -> "The type "++st True base++" could not be applied with "++st True arg++"\n"++indent msg)
				& unlines
		when (null rst) $ halt $ "No types left in the union. Tried types: \n"++indent msg
		return rst


-- Calculates a applied type. Gives the rest of the types, and new, calculated type requirements
calcType	:: Ctx -> RType -> RType -> Exc RType
calcType ctx (RCurry t0 rest) arg
	= do	let tt		= ctx & tables & typeTable
		let rqs		= ctx & reqs
		-- the argument can come from a different context, we thus rename frees
		let arg'	= substitute' (M.keys rqs) arg & fst
		binding		<- either halt return $ bind tt rqs arg' t0
		return $ substitute binding rest
calcType ctx t0 arg
	= halt $ "The type "++st True t0++" is applied to "++(arg & st True)++", but this is not possible"


preClean	:: Expression -> Expression
preClean (Seq exps)
		= exps & preClean' & Seq
preClean (Tuple exps)
		= exps & preClean' & Tuple
preClean e	= e

preClean'	:: [Expression] -> [Expression]
preClean' exps	= exps & filter (not . isExpNl) |> preClean
