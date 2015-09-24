module Languate.TAST where

{--

This module the TypeChecked-AST, the 'simpler' version of Languate.AST. It is the counterpart of Languate.AST.

In these data structure, all source-code things, docs, ... are stripped of. Only the typed program remains, which will be interpreted later.

It is this data-structure that all semantic analysis things use or build.

--}

import StdDef
import qualified HumanUtils
import Languate.AST
import Data.List
import Normalizable
import Languate.FQN
import Languate.MarkUp as Mu

import Data.Map
import Data.Maybe

import Control.Arrow



-- The (implicit) supertype for every type
anyType		= uncurry RNormal anyTypeID
anyTypeID	= (toFQN' "pietervdvn:Data:Any", "Any")

boolType	= uncurry RNormal boolTypeID
boolTypeID	= (toFQN' "pietervdvn:Data:Data.Bool", "Bool")

voidType	= uncurry RNormal voidTypeID
voidTypeID	= (toFQN' "pietervdvn:Data:Collection.Void","Void")

-- The representation of a tuple
tupleType	= uncurry RNormal tupleTypeID
tupleTypeID	= (toFQN' "pietervdvn:Data:Collection.Tuple","Tuple")

listType	= uncurry RNormal listTypeID
listTypeID	= (toFQN' "pietervdvn:Data:Collection.List","List")

setType	= uncurry RNormal setTypeID
setTypeID	= (toFQN' "pietervdvn:Data:Collection.Set","Set")

charType	= uncurry RNormal charTypeID
charTypeID	= (toFQN' "pietervdvn:Data:Data.Char", "Char")

natFQN		= toFQN' "pietervdvn:Data:Num.Nat"

natType		= uncurry RNormal natTypeID
natTypeID	= (natFQN, "Nat")

natType'	= uncurry RNormal natTypeID'
natTypeID'	= (natFQN, "Nat'")

-- Zero Constructor for the natural type
natTypeZero	:: TExpression
natTypeZero	= TCall ([natType],[]) $ Signature natFQN "Zero" [natType] []

natTypeSucc	:: TExpression
natTypeSucc	= TCall ([ RCurry natType natType'],[]) $
			Signature natFQN "Succ" [RCurry natType natType'] []

natTypeSucc'	:: TExpression -> TExpression
natTypeSucc'	= TApplication ([natType'], []) natTypeSucc

intType		= uncurry RNormal intTypeID
intTypeID	= (natFQN, "Int")

intType'	= uncurry RNormal intTypeID'
intTypeID'	= (natFQN, "Int'")


floatType	= uncurry RNormal floatTypeID
floatTypeID	= (toFQN' "pietervdvn:Data:Num.Float", "Float")

maybeType	= uncurry RNormal maybeTypeID
maybeTypeID	= (toFQN' "pietervdvn:Data:Collection.Maybe", "Maybe")



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
-- A function that is all of these. Note that frees are all regarded in the same context
type RTypeUnion		= [RType]
type RTypeReq		= (Name, ResolvedType)
type RTypeReqs		= [(Name, [ResolvedType])]

-- assumed that the treqs also contain all used free variables
type RTypeInfo		= (RTypeUnion, RTypeReqs)

{- a signature for a single implementation.
All types (union + reqs) live withing the same context, thus a free type var is the same everywhere
-}
data Signature		= Signature
				{ signFQN	:: FQN	-- where the function is defined
				, signName	:: Name	-- its name
				, signTypes	:: RTypeUnion	-- all the types it obeys
				, signTypeReqs	:: RTypeReqs	-- needed requirements
				}
	deriving (Show, Eq, Ord)

asSignature	:: (FQN, Name, RTypeUnion, RTypeReqs) -> Signature
asSignature (fqn, n, rtps, rtpreqs)
	= Signature fqn n rtps rtpreqs


data TypedExpression	= TFlt Float	| TChr Char	-- primitives
	{- The TApplication represents the first expression, applied on the second as argument. As multiple implementations with the same types exist, multiple types could be returned. A TApplication however representats only one of those, and selects those TExpressions which makes it possible. This way, a typed expression, has only one possible TypeUnion and one implementation to choose from. -}
			| TApplication (RTypeUnion, RTypeReqs) TypedExpression TypedExpression
			| TCall (RTypeUnion, RTypeReqs) Signature	-- we save the type independently as not to change the signature - we need it to look up the implementation
			| TLocalCall Name (RTypeUnion, RTypeReqs)
	deriving (Eq)
type TExpression	= TypedExpression

instance Show TypedExpression where
	show 	= showTE

-- Typed Expression to string
showTE	:: TypedExpression -> String
showTE (TFlt f)
	= show f ++ " :Float"
showTE (TChr c)
	= show c ++ " :Char"
showTE (TApplication (retTps, reqs) func arg)
	= let	funcStr	= HumanUtils.pars (show func)
		argStr	= HumanUtils.pars (show arg)
		tpStr	= show retTps in
		funcStr ++ " " ++ argStr ++ " :"++tpStr
showTE (TCall _ sign)
	= signName sign
showTE (TLocalCall nm _)
	= show nm


typeOf		:: TExpression -> (RTypeUnion, RTypeReqs)
typeOf (TFlt _)	= ([floatType], [])
typeOf (TChr _)	= ([charType], [])
typeOf (TCall typeInfo _)
		= typeInfo
typeOf (TApplication typeInfo _ _)
		= typeInfo
typeOf (TLocalCall _ t)
		= t


{-
A local scope keeps track of what variable has what type.
It is build based on the pattern matching of the function, while typing the patterns.
-}
type LocalScope	= Map Name RType
data TPattern	= TAssign Name
		| TDeconstruct Signature [TPattern]
		| TMulti [TPattern]
		| TDontCare
		| TEval TExpression	-- The value should be the same as the result of this expression
	deriving (Show, Eq)

data TClause		= TClause [TPattern] TExpression
	deriving (Show, Eq)
type FuncBody	= TClause

-------------------- Only utils, instance declaration and boring stuff below -------------------------------------



instance Show ResolvedType where
	show	= st False

st		:: Bool -> RType -> String
st short t0@(RNormal fqn str)
	| anyType == t0
		= ". "
	| voidType == t0
		= "() "
	| otherwise
		=  (if short then "" else show fqn++ "." ) ++ str
st short (RFree str)
		=  str
st short t0@(RApplied bt t)
	| bt == listType
		= "[" ++ showCommaSep short t ++ "]"
	| bt == setType
		= "{"++ showCommaSep short t ++"}"
	| otherwise
		= "("++ showCommaSep short t0 ++")"
st short (RCurry at rt)
		=  "(" ++ st short at ++ " -> " ++ st short rt ++")"


showCommaSep short t0@(RApplied bt at)
	= let 	btid	= getBaseTID bt
		special	= isJust btid && tupleTypeID == fromJust btid in
		if special then stuple short t0 else st short bt ++ " "++st short at
showCommaSep short t
	= st short t


stuple short t0@(RApplied (RApplied bt a) b)
	| bt	== tupleType
		= st short a ++", "++stuple short b
	| otherwise
		= st short t0
stuple short t	= st short t

isApplied	:: RType -> Bool
isApplied (RApplied _ _)	= True
isApplied _	= False


showRTypeReq	:: RTypeReq -> String
showRTypeReq (name, rtype)
		=  showRTypeReq' (name, [rtype])

showRTypeReq'	:: (Name, [RType]) -> String
showRTypeReq' (nm, subs)
		=  nm ++":" ++ intercalate ", " (Data.List.map (st True) subs)

rTypeReqs2md	:: RTypeReqs -> MarkUp
rTypeReqs2md rqs
		= rqs	|> showRTypeReq'
			|> code & Mu.Seq

instance Normalizable ResolvedType where
	normalize	= nt

nt	:: ResolvedType -> ResolvedType
nt (RApplied t0 t1)	= RApplied (nt t0) $ nt t1
nt (RCurry t0 t1)	= RCurry (nt t0) $ nt t1
nt t			= t



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

asRType'	:: (FQN, Name) -> RType
asRType' (fqn, nm)
		= RNormal fqn nm

isRFree (RFree _)	= True
isRFree _	= False

traverseRT	:: (RType -> RType) -> RType -> RType
traverseRT f (RApplied bt t)
		= RApplied (traverseRT f bt) $ traverseRT f t
traverseRT f (RCurry at rt)
		= RCurry (traverseRT f at) $ traverseRT f rt
traverseRT f t	= f t

foldRT	:: (RType -> a) -> ([a] -> a) -> RType -> a
foldRT f conct (RApplied bt t)
		= conct [foldRT f conct bt, foldRT f conct t]
foldRT f conct (RCurry at rt)
		= conct [foldRT f conct at, foldRT f conct rt]
foldRT f _ t	= f t


getBaseTID	:: RType -> Maybe (FQN, Name)
getBaseTID (RNormal fqn nm)
		= Just (fqn, nm)
getBaseTID (RApplied bt _)
		= getBaseTID bt
getBaseTID _	= Nothing

-- Means a base tid exists
isNormal	:: RType -> Bool
isNormal 	= isJust . getBaseTID


freesInRT	:: RType -> [Name]
freesInRT	=  nub . foldRT frees concat
			where 	frees	:: RType -> [Name]
				frees (RFree a)	= [a]
				frees _		= []

freesInReq	:: (Name, [RType]) -> [Name]
freesInReq (nm, tps)
		= nm:(tps >>= freesInRT) & nub

appliedTypes	:: RType -> [RType]
appliedTypes (RApplied bt at)
	= appliedTypes bt ++ [at]
appliedTypes _
	= []

curriedTypes	:: RType -> [RType]
curriedTypes (RCurry arg ret)
		= arg:curriedTypes ret
curriedTypes t	= [t]

uncurriedTypes	:: [RType] -> RType
uncurriedTypes []
		= error "Uncurrying types with empty list..."
uncurriedTypes [t]
		= t
uncurriedTypes (t:ts)
		= RCurry t $ uncurriedTypes ts

tupledTypes	:: RType -> [RType]
tupledTypes t@(RApplied (RApplied tupleT a) rest)
 | tupleT == tupleType
		= a:tupledTypes rest
 | otherwise	= [t]
tupledTypes t
 | t == voidType= []
 | otherwise	= [t]



-- Given "T x y z", gives a binding {a0 --> x, a1 --> y, a2 --> z}. Note: a0, a1, ... is hardcoded (defaultFreeNames)
canonicalBinding	:: RType -> Map Name RType
canonicalBinding t
	= appliedTypes t & zip defaultFreeNames & fromList

defaultFreeNames	:: [Name]
defaultFreeNames	= [0..] |> show |> ('a':)
