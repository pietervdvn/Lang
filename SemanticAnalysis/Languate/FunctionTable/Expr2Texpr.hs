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
expr2texpr	:: Package -> TableOverview -> FQN -> OperatorFreeExpression -> Exc [TExpression]
expr2texpr p to fqn e
	=  runstateT (_e2te $ normalize $ preClean e) (Ctx p to fqn) |> fst

data Ctx = Ctx	{ package	:: Package
		, tables	:: TableOverview
		, location	:: FQN	-- the current location where to search
		}

type SCtx a	= StateT Ctx (Exceptions String String) a

_e2te		:: Expression -> SCtx [TExpression]
_e2te (Nat n)	= returns $ TNat n
_e2te (Flt f)	= returns $ TFlt f
_e2te (Chr c)	= returns $ TChr c
_e2te (Call nm)	= do
	fqn		<- get' location
	funcTables	<- get' tables |> functionTables
				|> unpackFTS
	funcTable	<- lift $ M.lookup fqn funcTables ? errMsg fqn
	signs 	<- lift $ M.lookup nm (known funcTable) ? errMsg' fqn nm
	signs |> TCall & return
_e2te (Seq (function:args)) = do
	tfunctions	<- _e2te function
	calcApplications tfunctions args
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

calcApplications	:: [TExpression] -> [Expression] -> SCtx [TExpression]
calcApplications f []	= return f
calcApplications f (arg:args)
	= do	targ	<- _e2te arg
		applied	<- calcApplication f targ
		calcApplications applied args

{- Applied the argument on the given expression. Deducts the types, and returns those in a TApplication-}
calcApplication	:: [TExpression] -> [TExpression] -> SCtx [TExpression]
calcApplication fs args
	= do	let fsInfo	= fs |> (typeOf &&& id)
		let argsInfo	= args |> (typeOf &&& id)
		results		<- calcTypes fsInfo argsInfo
		results |> (\((funcExp, argExpr), tinfo) -> TApplication tinfo funcExp argExpr) & return

{- calculates the resulting types that a function has.

There is room for an extra parameter, (e.g. the signature) which will be passed.

-}
calcTypes	:: [(RTypeInfo, a)] -> [(RTypeInfo,a)] ->
			SCtx [((a,a), RTypeInfo)]
calcTypes func arg
	= do	let tps	= [ (((funcT, argT),(funcA,argA)), (funcT, argT))
				| (funcT, funcA) <- func, (argT, argA) <- arg]
		ctx	<- get
		let res	= tps ||>> uncurry (calcTypeUnion ctx) ||>> runExceptions ||>> thd3 -- :: [((([RTypeInfo], [RTypeInfo]), (a,a)), Either String RTypeInfo)]
		let smsg (((funcUnion, argUnion), _), Left msg)
			= "The typeunion "++ (funcUnion & fst |> st True & unwords)++
			" could not be applied on the types "++
			(argUnion & fst |> st True & unwords)++":\n"++indent msg
		let msg	= res |> smsg & unlines	:: String
		let ok	= res |> first snd
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
calcTypeUnion	:: Ctx -> RTypeInfo -> RTypeInfo -> Exc RTypeInfo
calcTypeUnion ctx (rtu, funcReqs) (rtuArg, argReqs)
	= do	let tps = [((baseType, funcReqs), (argType, argReqs))
				| baseType <- rtu, argType <- rtuArg]
				|> (id &&& uncurry (calcType ctx))
				||>> runExceptions ||>> thd3
		let rst	= tps |> snd & rights & unzip |> concat
				:: ([RType], RTypeReqs)
		let msg	= tps |> (\((base, arg), Left msg) -> "The type "++(base & fst & st True) ++" could not be applied with "++(arg & fst & st True)++"\n"++indent msg)
				& unlines
		when (null $ fst rst) $ halt $ "No types left in the union. Tried types: \n"++indent msg
		return rst


-- Calculates a applied type. Gives the rest of the types, and new resting type
calcType	:: Ctx -> (RType, RTypeReqs) -> (RType, RTypeReqs) -> Exc (RType, RTypeReqs)
calcType ctx (RCurry t0 rest, reqs) (arg, argReqs)
	= do	let tt		= ctx & tables & typeTable
		-- the argument can come from a different context, we thus rename frees
		let bindAway	= buildBinding' (reqs |> fst)
		let arg'	= substitute (asBinding bindAway) arg
		let argReqs'	= argReqs |> substituteReq bindAway	:: RTypeReqs
		-- the actual requirements
		let rqs		= merge (argReqs' ++ reqs) ||>> (nub . concat)	:: RTypeReqs
		binding		<- either halt return $ bind tt (M.fromList rqs) arg' t0
		return (substitute binding rest,
			rqs & filter ((`notElem` M.keys bindAway) . fst))
calcType ctx t0 arg
	= halt $ "The type "++(t0 & fst & st True)++" is applied to "++(arg & fst & st True)++", but this is not possible"


preClean	:: Expression -> Expression
preClean (Seq exps)
		= exps & preClean' & Seq
preClean (Tuple exps)
		= exps & preClean' & Tuple
preClean e	= e

preClean'	:: [Expression] -> [Expression]
preClean' exps	= exps & filter (not . isExpNl) |> preClean


returns a	= return [a]
