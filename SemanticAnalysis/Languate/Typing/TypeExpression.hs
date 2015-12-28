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
 		returns $ TLocalCall str ([rtype], unp reqs)
 | otherwise
 	= do	let visib	= visibleFuncs ft
		let available	= M.findWithDefault S.empty str visib
		if S.null available then do
			err $ show str ++ " is not known here.\n\t(Just like the study schedule of Iaason: 'Ik maak ook geen schemas' -- Iasoon)"
			return []
		else do
			let subAway	= buildSafeSubs reqs
			let signs	= available & S.toList |> fst
			types	<- (zip signs signs |> first signTypes) |+> (onFirst $ subsUnion subAway)	:: Exc [(CTypeUnion, Signature)]
			return (types |> uncurry TCall)
typeExpr (ft, tt) reqs ls (Seq [exprs])
	= todo
typeExpr _ _ _ (Operator str)
	= halt $ "BUG: the expression is not passed through the proper rewrites to remove operators and bring it in prefix form"

typeExpr _ _ _ expr
	=  do	warn $ "TODO: Expr " ++ show expr
		-- TODO
		returns $ TLocalCall ("") ([],[])


returns	:: Monad m => a -> m [a]
returns	= return . (:[])
