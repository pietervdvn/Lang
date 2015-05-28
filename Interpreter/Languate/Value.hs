module Languate.Value where

import StdDef

import Languate.FQN
import Languate.TAST
import Languate.Semantal
import Languate.TableOverview

import Data.Map as M

{--
A value is either:

- A ADT in weak head normal form. This means that the constructor is visible
- An unevaluated expression, with the context in which it should be evaluated
--}
data Value	= ADT Int RTypeInfo [Value]
		| Thunk [(Context, TClause)]	-- something that still has to be evaluated

data Context	= Ctx {	package		:: TableOverview
		  	, location	:: FQN
			, localScope	:: Map Name Value}

-- Dependency injection
data Evaluators	= Evaluators {
			evalExpr'	:: Context -> TExpression -> Value,
			evalPattern'	:: Context -> Value -> TPattern -> Maybe (Map Name Value)}


instance Show Value where
	show 	= sv


sv	:: Value -> String
sv (ADT i t vals)
	= "ADT: "++show i++" <"++show t++"> " ++ show vals
sv (Thunk texpr)
	= "THUNK: " ++ (texpr |> snd & show)

instance Eq Value where
	(==)	= eq

eq	:: Value -> Value -> Bool
eq (ADT i t args) (ADT i' t' args')
	= i == i' && t == t' && args == args'
eq _ _	= False
