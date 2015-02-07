module Languate.TAST where

{--

This module the TypeChecked-AST, the 'simpler' version of Languate.AST. It is the counterpart of Languate.AST.

In these data structure, all source-code things, docs, ... are stripped of. Only the typed program remains, which will be interpreted later.

It is this data-structure that all semantic analysis things use or build.

--}

import StdDef
import Languate.AST
import Data.List
import Normalizable
import Languate.FQN

import Data.Map

{- Each type has a kind. You can think of those as the 'type of the type'
E.g. a 'Maybe' is always in function of a second argument, e.g. 'Maybe Int' or 'Maybe String'.
'String' and ''Functor Int'' have a kind ''a'' (no arguments), whereas ''Functor'' has a kind ''a -> b''
Type requirements are **not** stored into the kind, these are checked seperatly.

e.g.
type True a b	= a	:: * ~> * ~> *
type False a b	= b	:: * ~> * ~> *
type And b1 b2 a b = b2 (b1 a b) b
			:: (* ~> * ~> *) ~> (* ~> * ~> *) ~> *

-}
data Kind		= Kind
			| KindCurry Kind Kind -- Kind curry: Dict a b :: a ~> (b ~> *)
	deriving (Ord, Eq)


-- resoved type -- each type known where it is defined
data ResolvedType	= RNormal FQN Name
			| RFree String
			| RApplied RType RType
			| RCurry RType RType
	deriving (Eq, Ord)
type RType		= ResolvedType
type RTypeReq		= (Name, ResolvedType)


data TypedExpression	= TNat Int	| TFlt Float	| TChr Char	-- primitives
			{- the first argument, [RType] are all the possible **return** types. E.g. '(&&) True False' -> Call [Bool] "&&" [..., ...]; '(&&) True' -> Call [Bool -> Bool] -}
			| TApplication [RType] TypedExpression [TypedExpression]
			| TCall [RType] Name
	deriving (Show, Eq)
type TExpression	= TypedExpression

data TPattern	= TAssign Name
		| TDeconstruct (Name, RType) [TPattern]
		| TMulti [TPattern]
		| TDontCare
		| TEval TExpression
	deriving (Show, Eq)

data TClause		= TClause [TPattern] TExpression
	deriving (Show, Eq)


-------------------- Only utils, instance declaration and boring stuff below -------------------------------------

instance Show ResolvedType where
	show	= st False

st		:: Bool -> RType -> String
st short (RNormal fqn str)
		=  (if short then "" else show fqn++ "." ) ++ str
st short (RFree str)
		=  str
st short (RApplied bt t)
		=   "(" ++ st short bt ++ " " ++ st short t ++")"
st short (RCurry at rt)
		=  "(" ++ st short at ++ " -> " ++ st short rt ++")"


isApplied	:: RType -> Bool
isApplied (RApplied _ _)	= True
isApplied _	= False


showRTypeReq	:: RTypeReq -> String
showRTypeReq (name, rtype)
		=  showRTypeReq' (name, [rtype])

showRTypeReq'	:: (Name, [RType]) -> String
showRTypeReq' (nm, subs)
		=  nm ++":" ++ intercalate ", " (Data.List.map (st True) subs)

instance Normalizable ResolvedType where
	normalize	= nt

nt	:: ResolvedType -> ResolvedType
nt (RApplied t0 t1)	= RApplied (nt t0) $ nt t1
nt (RCurry t0 t1)	= RCurry (nt t0) $ nt t1
nt t			= t




typeOf		:: TypedExpression -> [RType]
typeOf (TNat _)	=  [nat, int]
typeOf (TFlt _)
		=  [float]
typeOf (TChr _)	=  [RNormal (toFQN' "pietervdvn:Data:Data.Char") "Char"]
typeOf (TCall tps _)
		=  tps
typeOf (TApplication tps _ _)
		=  tps

nat	= num "Nat"
int	= num "Int"
float	= num "Float"

num str	= RNormal (toFQN' $ "pietervdvn:Data:Num."++str) str

instance Show Kind where
	show Kind	= "*"
	show (KindCurry arg0 arg1)
			= "(" ++ show arg0 ++ " ~> " ++ show arg1 ++ ")"


normalKind	:: Kind -> Bool
normalKind Kind	= True
normalKind _	= False

numberOfKindArgs	:: Kind -> Int
numberOfKindArgs Kind	= 0
numberOfKindArgs (KindCurry _ k)
			= 1 + numberOfKindArgs k
-- simple conversion, only use for type declarations. E.g. '''data Bool = ...''' in module '''Data.Bool''': '''asRType (FQN ["Data"] "Bool")(Normal "Bool") '''
asRType	:: FQN -> Type -> RType
asRType fqn (Normal [] nm)
	= RNormal fqn nm

traverseRT	:: (RType -> RType) -> RType -> RType
traverseRT f (RApplied bt t)
		= RApplied (traverseRT f bt) $ traverseRT f t
traverseRT f (RCurry at rt)
		= RCurry (traverseRT f at) $ traverseRT f rt
traverseRT f t	= f t

foldRT	:: (RType -> a) -> ([a] -> a) -> RType -> a
foldRT f concat (RApplied bt t)
		= concat [foldRT f concat bt, foldRT f concat t]
foldRT f concat (RCurry at rt)
		= concat [foldRT f concat at, foldRT f concat rt]
foldRT f _ t	= f t
