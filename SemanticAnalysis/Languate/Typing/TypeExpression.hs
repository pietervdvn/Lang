module Languate.Typing.TypeExpression where

import StdDef
import Exceptions
import Languate.CheckUtils

import Languate.AST
import Languate.TAST
import Languate.FQN
import Languate.FunctionTable.Def
import Languate.Typetable

import Data.Set as S
import Data.Map as M
import Data.List as L

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
	= do	rec	<- typeExpr t r ls (Nat $ i - 1)	:: Exc [TExpression]
		rec |> natTypeSucc' & return
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
typeExpr tables@(ft, tt) reqs ls (Seq (function:arg:rest))
	= do	f'	<- typeExpr tables reqs ls function
		arg'	<- typeExpr tables reqs ls arg
		-- we build all possible combinations of function arg application
		let fargs	= [(f, arg) | f <- f', arg <- arg']
		-- we apply each of the types; we keep the combinations which can be bound
		-- TODO pickup0
		halt $ "TODO: application of "++show f' ++ " on "++show arg'
typeExpr _ _ _ (Operator str)
	= halt $ "BUG: the expression is not passed through the proper rewrites to remove operators and bring it in prefix form"

typeExpr _ _ _ expr
	=  do	warn $ "TODO: Expr " ++ show expr
		-- TODO
		return [TLocalCall (RFree "a") "TODO"]


applyExpression	:: Expression -> Expression -> Maybe Expression
applyExpression f arg
	= Nothing	-- TODO pickup 1



{- Gives the result of applying the second type to the first.
	e.g.
	applyType (Int -> Bool) Int	= Bool
	applyType (Bool -> Bool) Int 	= --- NOTHING ---
	applyType (a -> List a) Int	= List Int
	This is done by binding (function type -> function result) against (argType -> _resultType); binding will bind _resultType against the resulting type.
	Note that all requirements should be fullfilled by the context
-}
applyTypes	:: RType -> RType -> Maybe RType
applyType funcType argType
	= Nothing	-- TODO pickup 2

returns	:: Monad m => a -> m [a]
returns	= return . (:[])
