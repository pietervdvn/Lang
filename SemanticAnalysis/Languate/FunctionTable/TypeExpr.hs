module Languate.FunctionTable.TypeExpr where

import StdDef hiding (isRight)
import Exceptions
import Normalizable
import HumanUtils hiding (when)

import Graphs.DirectedGraph
import Graphs.TreeShake


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
import Languate.Precedence.Expr2PrefExpr

import qualified Languate.BuiltIns as BuiltIn

import Data.Map (Map, findWithDefault)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.List (nub)
import Data.Either
import Data.Tuple
import Data.Maybe
import Data.Char


import StateT
import Control.Monad
import Control.Monad.Trans
import Control.Arrow

import Debug.Trace

{-

Welcome to the lambda core

This is the heart of the typechecker, where expressions get converted into their typed counterpart.

-}

{-
Builds a typed expression from a AST expression. This involves both typechecking and resolving the exact method.

The requirements should contain all used free type variables within the context

-}
expr2texpr	:: Package -> TableOverview -> FQN -> [Name] -> Map Name (RTypeUnion, RTypeReqs) -> OperatorFreeExpression -> Exc [TExpression]
expr2texpr p to fqn frees localScope e
	=  do	tlt	<- to & typeTable & typeLookups & M.lookup fqn ? ("No tlt found for "++show fqn)
		let ctx	= Ctx p to fqn localScope (S.fromList frees) tlt
		runstateT (_e2te $ normalize $ expr2prefExpr (precedenceTable to) $ preClean e) ctx |> fst

data Ctx = Ctx	{ package	:: Package
		, tables	:: TableOverview
		, location	:: FQN		-- the current location where to search
		, localScope	:: Map Name (RTypeUnion, RTypeReqs)	-- Locally defined variables
		, knownFrees	:: Set Name	-- The frees which are used within this context.
		, typeLookupT	:: TypeLookupTable
		}
type SCtx a	= StateT Ctx (Exceptions String String) a

_e2te		:: Expression -> SCtx [TExpression]
_e2te (Nat 0)	= returns natTypeZero
_e2te (Nat n)	= _e2te (Nat (n-1)) ||>> natTypeSucc'
_e2te (Flt f)	= returns $ TFlt f
_e2te (Chr c)	= _e2te (Nat $ ord c) ||>> charTypeConstr'
_e2te (Call nm) = do
	scope	<- get' localScope
	if nm `M.member` scope then do
		t	<- lift $ M.lookup nm scope ? (nm++" should be in the local scope")
		returns $ TLocalCall nm t
	else do
		fqn		<- get' location
		funcTables	<- get' tables |> functionTables
					|> unpackFTS
		funcTable	<- lift $ M.lookup fqn funcTables ? errMsg fqn
		signs 	<- lift $ M.lookup nm (known funcTable) ? errMsg' fqn nm
		signs'	<- mapM escapeSign signs
		let tpInf sign'	= cleanType (signTypes sign', signTypeReqs sign')
		zip signs signs' |> (\(origSign, sign') -> TCall (tpInf sign') origSign)
			& return
_e2te (Seq (BuiltIn "construct" resultType:Nat i:args))
	= do	rtype	<- resolveTps resultType
		let nrOfArgs	= length args
		let constructor	= BuiltIn.constructTCall rtype nrOfArgs
		let argNames	= args |> (\(Call nm) -> nm)
		mapM_ addLocalVar argNames
		calcApplications [constructor] (Nat i : args)
_e2te (Seq [BuiltIn "deconstruct" funcType, Nat i, Call val])
	= do	rtype@(funcRT, reqs)	<- resolveTps funcType
		let (RCurry valType _)	= funcRT
		addLocalVar' val valType
		let destructor	= BuiltIn.destructTCall rtype
		calcApplications [destructor] [Nat i, Call val]
_e2te (Seq [BuiltIn "is" valType, Nat i, Call val])
	= do	rtype@(valRType, reqs)	<- resolveTps valType
		let is	= BuiltIn.isTCall rtype
		addLocalVar' val valRType
		calcApplications [is] [Nat i, Call val]
_e2te seq@(Seq (function:args)) = do
	-- types are renamed at this point, thus no further escape is needed
	tfunctions	<- _e2te function
	calcApplications tfunctions args
_e2te (Tuple [])
	= returns voidTypeCons
_e2te (Tuple [a])
		= _e2te a
_e2te (Tuple [a,b])
		= _e2te $ Seq [tupleCall, a ,b]
_e2te (Tuple (a:as))
		= _e2te $ Seq [tupleCall, a, Tuple as]
_e2te e		=
	lift $ halt $ "Could not type the expression "++show e++"\n\t"++
	case e of
		ExpNl _	-> "A comment/expnl slipped through the clean"
		Operator _
			-> "Seems like the expression didn't go through prefix rewritting (to remove ops)"
		Cast _	-> "Casts are for a next version :p"
		AutoCast-> "Autocasts are for a next verion :p"
		BuiltIn _ _
			-> "Some builtin slipped through... This is a bug"


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
		results |> (\((funcExp, argExpr), tinfo) -> TApplication (cleanType tinfo) funcExp argExpr) & return

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

escapeSign	:: Signature -> SCtx Signature
escapeSign sign
	= do	bindAway 	<- get' knownFrees |> S.toList |> buildBinding'
		let tps'	= signTypes sign |> substitute (asBinding bindAway)
		let tpReqs'	= signTypeReqs sign |> substituteReq bindAway
		let frees	= (tps' >>= freesInRT)  ++ (tpReqs' |> snd & concat >>= freesInRT) ++ (tpReqs' |> fst)	:: [Name]
		addFrees frees
		return $ sign {signTypes = tps', signTypeReqs = tpReqs'}



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
		return $ first nub rst


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
		let bindingReqs	= binding & unbind & M.toList & merge
		let newRqs	= (rqs ++ bindingReqs) & merge ||>> concat
		return (rest, newRqs )
calcType ctx t0 arg
	= halt $ "The type "++(t0 & fst & st True)++" is applied to "++(arg & fst & st True)++", but this is not possible"





cleanType	:: RTypeInfo -> RTypeInfo
cleanType inf@(rtpUnion, reqs)
	= let	-- all known frees
		frees		= (rtpUnion >>= freesInRT) ++ (reqs >>= freesInReq)
		fullReqs	= (zip frees [] ++ reqs) & merge ||>> concat
					& filter (not . idiotReq)	:: RTypeReqs
		singleReqs	= fullReqs & filter ((==) 1 . length . snd) ||>> head
		binding		= Binding $ M.fromList singleReqs
		rtpUnion'	= rtpUnion |> substitute binding
		reqs'		= fullReqs |||>>> substitute binding
		rootFrees	= rtpUnion' >>= freesInRT
		freeDepGraph	= reqs' |||>>> freesInRT ||>> concat & M.fromList |> S.fromList
		neededFrees	= treeshake (S.fromList rootFrees) $ makeComplete freeDepGraph
		cleanReqs	= reqs' & filter ((`M.member` neededFrees) . fst) in
		(rtpUnion', cleanReqs)


-- true is the given type requirement is idiotic/trivial: ("a", RFree a) is idiotic
idiotReq	:: (Name, [RType]) -> Bool
idiotReq (a, [RFree b])
		= a == b
idiotReq _	= False

-- expression cleanup before actual typing
preClean	:: Expression -> Expression
preClean (Seq exps)
		= exps & preClean' & Seq
preClean (Tuple exps)
		= exps & preClean' & Tuple
preClean e	= e

preClean'	:: [Expression] -> [Expression]
preClean' exps	= exps & filter (not . isExpNl) |> preClean


returns a	= return [a]

addFrees	:: [Name] -> SCtx ()
addFrees nms	= do	ctx	<- get
			put $ ctx {knownFrees = S.union (S.fromList nms) $ knownFrees ctx}

-- adds a local variable with any type. For use with constructors only
addLocalVar	:: Name -> SCtx ()
addLocalVar nm	= do	ctx 	<- get
			put $ ctx {localScope = M.insert nm ([anyType],[]) $ localScope ctx}

addLocalVar'	:: Name -> RType -> SCtx ()
addLocalVar' nm	tp
	= do	ctx 	<- get
		put $ ctx {localScope = M.insert nm ([tp],[]) $ localScope ctx}

resolveTps	:: (Type, [(Name, Type)]) -> SCtx (RType, RTypeReqs)
resolveTps (t,treqs)
		= do	tlt	<- get' typeLookupT
			rtp	<- lift $ resolveType tlt t
			rtreqs	<- lift $ mapM (resolveTypeIn tlt) treqs
			return (rtp, rtreqs & merge)
