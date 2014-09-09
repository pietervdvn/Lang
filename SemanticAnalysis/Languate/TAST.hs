module Languate.TAST where

{--

This module the TypeChecked-AST, the 'simpler' version of Languate.AST as all source-code things, docs, ... are stripped of. Only the program remains, to be interpreted later.

It is this data-structure that all semantic analysis things use.

--}

import StdDef
import Languate.AST
import Languate.Signature	

data TypedExpression	= TNat Int	| TFlt Float	| TChr Char	-- primitives
			{- the first argument, [Type] are all the possible **return** types. E.g. '(&&) True False' -> Call [Bool] "&&" [..., ...]; '(&&) True' -> Call [Bool -> Bool] -}
			| TApplication [Type] TypedExpression [TypedExpression]
			| TCall [Type] Name	
	deriving (Show)
type TExpression	= TypedExpression

data TPattern	= TAssign Name
		| TDeconstruct Signature [TPattern]
		| TMulti [TPattern]
		| TDontCare
		| TEval TExpression
	deriving (Show)

data TClause		= TClause [TPattern] TExpression
	deriving (Show)


typeOf		:: TypedExpression -> [Type]
typeOf (TNat _)	=  [Normal "Nat", Normal "Int"]
typeOf (TFlt _)
		=  [Normal "Float"]
typeOf (TChr _)	=  [Normal "Char"]
typeOf (TCall tps _)
		=  tps
typeOf (TApplication tps _ _)
		=  tps

