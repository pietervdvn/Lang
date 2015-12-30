module Languate.Typing.TypeExpression where

import StdDef
import HumanUtils
import Exceptions
import Languate.CheckUtils

import Languate.AST
import Languate.TAST
import Languate.FQN
import Languate.FunctionTable.Def
import Languate.Typetable
import Languate.TypeConstraint

import Data.Set as S
import Data.Map as M
import Data.List as L
import Data.Maybe
import Data.Char

import Control.Arrow


{-

Types an expression within context (tables, requirements on free types and localscope).

Note that an expression might result in multiple typed expressions, as multiple possibilities exist.
(e.g. (+) : Nat -> Nat -> Nat or Int -> Int -> Int).
Typing will return *both* options when a '+' is encountered and will remove the options that are not needed/possible, based on the types of the parameters/return value
-}
typeExpr	:: (FunctionTable, Typetable) -> FullRTypeReq -> LocalScope -> Expression -> Exc [TExpression]
typeExpr _ _ _ (Nat 0)
	= returns natTypeZero
typeExpr t r ls (Nat i)
	= do	natTypeSucc' (Tag $ i - 1) & returns
typeExpr t r ls (Chr c)
	= do	let i	= ord c
		[nat]	<- typeExpr t r ls (Nat i)
		returns $ charTypeConstr' nat
typeExpr (ft, _) reqs ls (Call str)
 | str `M.member` ls
 	= do	let (Just rtype)	= M.lookup str ls
 		returns $ TLocalCall rtype str
 | otherwise
 	= do	let visib	= visibleFuncs ft
		let available	= M.findWithDefault S.empty str visib
		if S.null available then do
			err $ show str ++ " is not known here.\n\t(Just like the study schedule of Iaason: 'Ik maak ook geen schemas' -- Iasoon)"
			return []
		else do
			let subAway	= buildSafeSubs reqs ||>> RFree
			let signs	= available & S.toList |> fst
			types	<- (zip signs signs |> first (fst . signType))
					|+> (onFirst $ subs subAway)	:: Exc [(RType, Signature)]
			return (types |> uncurry TCall)
typeExpr tables reqs ls (Seq [arg])
	= halt $ "Compiler bug: expressions to type are not normalized: "++show arg
typeExpr tables reqs ls (Seq (function:args))
	= do	f'	<- typeExpr tables reqs ls function
		applyExpression' tables reqs ls f' args

typeExpr _ _ _ (Operator str)
	= halt $ "BUG: the expression is not passed through the proper rewrites to remove operators and bring it in prefix form"

typeExpr _ _ _ expr
	=  do	warn $ "TODO: Expr " ++ show expr
		-- TODO
		return [TLocalCall (RFree "a") "TODO"]


applyExpression'	:: (FunctionTable, Typetable) -> FullRTypeReq -> LocalScope -> [TExpression] -> [Expression] -> Exc [TExpression]
applyExpression' _ _ _ func []	= return func
applyExpression' tables@(ft, tt) reqs ls func (arg:args)
	= inside ("While calculating the type of "++(func |> (\te -> show te ++ ": "++show (typeOf te)) & commas)++" on "++((arg:args) |> show & unwords )) $
	  do	arg'	<- typeExpr tables reqs ls arg
		-- we build all possible combinations of function arg application
		let fargs	= [(f, arg) | f <- func, arg <- arg']
		-- we apply each of the types; we keep the combinations which can be bound
		applied	<- fargs |+> uncurry (applyExpression tt reqs)
				|> catMaybes


		errIf (L.null applied) $ "No typing is possible for "++show func++" "++show arg++"\nTried:"
				++indent (fargs >>= (\(f, arg) -> "\n"++ show f  ++ ": "++show (typeOf f)++"\n\t with: " ++ show arg ++"\n\t of type "++show (typeOf arg)) )
		-- as soon as two expressions yield the same type, we should choose an implementation.
		applied'	<- chooseImplementations applied
		applyExpression' tables reqs ls applied' args

applyExpression	:: Typetable -> FullRTypeReq -> TExpression -> TExpression -> Exc (Maybe TExpression)
applyExpression tt reqs f arg
	= do	let ft	= typeOf f
		let argT	= typeOf arg
		resultT	<- applyTypes tt reqs ft argT
		case resultT of
			Nothing -> return Nothing
			(Just t)-> return $ Just $ TApplication t f arg


chooseImplementations	:: [TExpression] -> Exc [TExpression]
chooseImplementations [texpr]
			= returns texpr
chooseImplementations exprs
	= do	let sortedExprs	= exprs |> (typeOf &&& id) & sortOn fst
		-- when all expressions have exactly the same type, we choose an implementation
		{- this implementation uses the *most specific* function possible
			(thus with the smallest input parameters, as this functions can make more assumptions about it's inputs and will thus be the fastest)
		-}
		-- TODO
		-- halt $ show sortedExprs
		return exprs



{- Gives the result of applying the second type to the first.
	e.g.
	applyType (Int -> Bool) Int	= Bool
	applyType (Bool -> Bool) Int 	= --- NOTHING ---
	applyType (a -> List a) Int	= List Int
	This is done by binding (function type -> function result) against (argType -> _resultType); binding will bind _resultType against the resulting type.
	Note that all requirements should be fullfilled by the context, only a single constraint should thus rest
-}
applyTypes	:: Typetable -> FullRTypeReq -> RType -> RType -> Exc (Maybe RType)
applyTypes tt reqs funcType argType
	= do	let reqs'	= asConstraints $ unp reqs
		let binder	= RCurry argType (RFree "_resultType")
		let usedFrees	= "_resultType" : (unp reqs |> fst & nub)
		maybeConstr	<- allNeededConstraintsFor tt reqs' (bind funcType binder)
					||>> S.toList
		maybeConstr'	<- flattenMaybeT (maybeConstr |> buildBoundConstr ("_resultType" ==))
					||>> (removeUselessBinds usedFrees)
		case maybeConstr' of
			(Just [SubTypeConstr tp (RFree "_resultType")])
				-> return $ Just tp
			(Just extraConstraints)
				-> inside ("With requirements "++show (nub $ unp reqs)) $ halt $ "EXTRA " ++ show extraConstraints
			Nothing	-> return Nothing

returns	:: Monad m => a -> m [a]
returns	= return . (:[])


flattenMaybeT	:: Maybe (Exc (Maybe a)) -> Exc (Maybe a)
flattenMaybeT (Just exc)
		= exc
flattenMaybeT _	= return Nothing
