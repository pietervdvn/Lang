module Languate.Value where

import StdDef
import HumanUtils

import Languate.FQN
import Languate.TAST
import Languate.Semantal
import Languate.TableOverview
import Data.Char (ord, chr)

import Data.Map as M
import Data.Maybe

{--
A value is either:

- A ADT in weak head normal form. This means that the constructor is visible
- An unevaluated expression, with the context in which it should be evaluated
--}
data Value	= ADT Int RTypeInfo [Value]
		| Thunk [(Context, TClause)]	-- something that still has to be evaluated

data Context	= Ctx {	package		:: TableOverview
		  	, location	:: FQN
			, localScope	:: Map Name Value
			, stack		:: [String]}


printStackTrace	:: String -> Context -> a
printStackTrace msg ctx
	= error ("\n\n!!!!!!!!!!!!!!!!!!!\n"++msg ++ ":\n" ++ stack ctx & showStackTrace & unlines)

showStackTrace	:: [String] -> [String]
showStackTrace signs
		= signs |> ("In the function "++) ++ ["End of stack"]

-- Dependency injection
data Evaluators	= Evaluators {
			evalExpr'	:: Context -> TExpression -> Value,
			evalPattern'	:: Context -> Value -> TPattern -> Maybe (Map Name Value)}


instance Show Value where
	show 	= sv


sv	:: Value -> String
sv val@(ADT i t vals)
	= "ADT: "++show i++" <"++show t++"> " ++ show vals
sv (Thunk texpr)
	= "THUNK: " ++ (texpr |> snd & show)

-- shows a string value
showStringValue	:: Value -> String
showStringValue (ADT _ _ [])
		= ""
showStringValue (ADT _ _ (ADT _ _ [i]:[rest]))
		= chr (extractNatValue i) : showStringValue rest


-- extracts the integer value of encoded, natural values as (Succ (Succ Zero))
extractNatValue	:: Value -> Int
{-- no arguments means either
	- the value was in-lang constructed with 'Zero', ADT 0 Nat []
	- the value was natively built, e.g. 128 -> ADT 128 Nat []
--}
extractNatValue (ADT i _ [])	= i
-- The constructor 'Succ' was used
extractNatValue (ADT 1 _ [val])
	= 1 + extractNatValue val
extractNatValue v
	= error $ "This is a strange NAT-value: "++show v


instance Eq Value where
	(==)	= eq

eq	:: Value -> Value -> Bool
eq (ADT i t args) (ADT i' t' args')
	= i == i' && t == t' && args == args'
eq _ _	= False

typesOfVal	:: Value -> RTypeUnion
typesOfVal 	= fst . typeOfVal

typeOfVal	:: Value -> RTypeInfo
typeOfVal (ADT _ inf vals)
			= inf
typeOfVal (Thunk _)	= error "Type of thunk?"
