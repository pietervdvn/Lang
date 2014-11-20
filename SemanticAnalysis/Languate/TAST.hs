module Languate.TAST where

{--

This module the TypeChecked-AST, the 'simpler' version of Languate.AST. It is the counterpart of Languate.AST.

In these data structure, all source-code things, docs, ... are stripped of. Only the typed program remains, which will be interpreted later.

It is this data-structure that all semantic analysis things use or build.

--}

import StdDef
import Languate.AST
import Languate.Signature
import Data.List

data Kind		= Kind Name
			| KindCurry [Kind] Kind
	deriving (Ord, Eq)

data TypedExpression	= TNat Int	| TFlt Float	| TChr Char	-- primitives
			{- the first argument, [Type] are all the possible **return** types. E.g. '(&&) True False' -> Call [Bool] "&&" [..., ...]; '(&&) True' -> Call [Bool -> Bool] -}
			| TApplication [Type] TypedExpression [TypedExpression]
			| TCall [Type] Name
	deriving (Show, Eq)
type TExpression	= TypedExpression

data TPattern	= TAssign Name
		| TDeconstruct Signature [TPattern]
		| TMulti [TPattern]
		| TDontCare
		| TEval TExpression
	deriving (Show, Eq)

data TClause		= TClause [TPattern] TExpression
	deriving (Show, Eq)


typeOf		:: TypedExpression -> [Type]
typeOf (TNat _)	=  [Normal "Nat", Normal "Int"]
typeOf (TFlt _)
		=  [Normal "Float"]
typeOf (TChr _)	=  [Normal "Char"]
typeOf (TCall tps _)
		=  tps
typeOf (TApplication tps _ _)
		=  tps

instance Show Kind where
	show (Kind nm)	= nm
	show (KindCurry args arg)
			= "(" ++ intercalate " ~> " (map show $ args++[arg]) ++ ")"


normalKind	:: Kind -> Bool
normalKind (Kind _)	= True
normalKind _	= False
