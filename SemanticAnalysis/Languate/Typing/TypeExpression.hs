module Languate.Typing.TypeExpression where

import StdDef
import Exceptions
import Languate.CheckUtils

import Languate.AST
import Languate.TAST
import Languate.FQN

typeExpr	:: Expression -> Exc TExpression
typeExpr (Nat 0)= return natTypeZero
typeExpr (Nat i)= do	rec	<- typeExpr (Nat $ i - 1)
			return $ natTypeSucc' rec
typeExpr expr	=  return $ TLocalCall ("TODO: "++show expr) ([],[]) 
