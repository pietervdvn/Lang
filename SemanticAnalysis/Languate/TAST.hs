module Languate.TAST where

{--

This module the TypeChecked-AST, the 'simpler' version of Languate.AST. It is the counterpart of Languate.AST.

In these data structure, all source-code things, docs, ... are stripped of. Only the typed program remains, which will be interpreted later.

It is this data-structure that all semantic analysis things use or build.

--}

import StdDef
import HumanUtils (commas, pars, intercal)
import Exceptions
import Languate.CheckUtils
import Languate.AST
import Normalizable
import Languate.FQN
import Languate.MarkUp as Mu

import State

import Data.List as L
import Data.Map
import Data.Maybe

import Control.Arrow



-- The (implicit) supertype for every type
anyType		= uncurry RNormal anyTypeID
anyTypeID	= (toFQN' "pietervdvn:Data:Any", "Any")

boolType	= uncurry RNormal boolTypeID
boolTypeID	= (toFQN' "pietervdvn:Data:Data.Bool", "Bool")

voidType	= uncurry RNormal voidTypeID
voidTypeID	= (voidTypeFQN,"Void")
voidTypeFQN	= toFQN' "pietervdvn:Data:Collection.Void"
voidTypeCons	:: TExpression
voidTypeCons	= TCall ([voidType], []) $ Signature voidTypeFQN "Void" [voidType] []

-- The representation of a tuple
tupleType	= uncurry RNormal tupleTypeID
tupleType'	:: String -> String -> RType
tupleType' a b	= RApplied (RApplied tupleType $ RFree a) $ RFree b
tupleTypeID	= (tupleTypeFQN,"Tuple")
tupleCall	= Call "Tuple"
tupleTypeFQN	= toFQN' "pietervdvn:Data:Collection.Tuple"
tupleTypeCons	:: TExpression
tupleTypeCons	= let tp = RCurry (RFree "a")
			  (RCurry (RFree "b")
			  (tupleType' "a" "b")) in
			 TCall ([ tp ],[]) $
		     Signature tupleTypeFQN "Tuple" [tp] []

listType	= uncurry RNormal listTypeID
listTypeID	= (toFQN' "pietervdvn:Data:Collection.List","List")

setType	= uncurry RNormal setTypeID
setTypeID	= (toFQN' "pietervdvn:Data:Collection.Set","Set")

charType	= uncurry RNormal charTypeID
charTypeID	= (charTypeFQN, "Char")
charTypeFQN		= toFQN' "pietervdvn:Data:Data.Char"
charTypeConstr	= TCall ([ RCurry natType charType],[]) $
		     Signature charTypeFQN "Char" [RCurry natType charType] []
charTypeConstr'	= TApplication ([charType], []) charTypeConstr



natFQN		= toFQN' "pietervdvn:Data:Data.Nat"

natType		= uncurry RNormal natTypeID
natTypeID	= (natFQN, "Nat")

natType'	= uncurry RNormal natTypeID'
natTypeID'	= (natFQN, "Nat'")

-- Zero Constructor for the natural type
natTypeZero	:: TExpression
natTypeZero	= TCall ([natType],[]) $ Signature natFQN "Zero" [natType] []

natTypeSucc	:: TExpression
natTypeSucc	= TCall ([ RCurry natType natType'],[]) $
			Signature natFQN "Succ" [RCurry natType natType] []

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

stringType	= RApplied listType charType




type TypeID	= (FQN, Name)

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

-- Constrainded type
type CType		= (RType, RTypeReqs)
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




data TypedExpression	= TFlt Float	-- primitives
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
showTE (TApplication (retTps, reqs) func arg)
	= let	funcStr	= pars (show func)
		argStr	= pars (show arg)
		tpStr	= show retTps in
		funcStr ++ " " ++ argStr ++ " :"++tpStr
showTE (TCall _ sign)
	= signName sign
showTE (TLocalCall nm _)
	= show nm


typeOf		:: TExpression -> (RTypeUnion, RTypeReqs)
typeOf (TFlt _)	= ([floatType], [])
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
		=  (if short then "" else show fqn++ "." ) ++ str
st short (RFree str)
		=  str
st short t0@(RApplied bt t)
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
		=  nm ++":" ++ intercalate ", " (fmap (st True) subs)

rTypeReqs2md	:: RTypeReqs -> MarkUp
rTypeReqs2md rqs
		= rqs	|> showRTypeReq'
			|> code & Mu.Seq

instance Normalizable ResolvedType where
	normalize rt	= normalizeRT "a" rt & fst

normalizeRT names rt	= runstate (nt rt) (defaultFreeNames' names, []) |> snd

nt	:: ResolvedType -> State ([Name],[(Name, Name)]) ResolvedType
nt (RApplied t0 t1)	= do	t0'	<- nt t0
				t1'	<- nt t1
				return $ RApplied t0' t1'
nt (RCurry t0 t1)	= do	t0'	<- nt t0
				t1'	<- nt t1
				return $ RCurry t0' t1'
nt (RFree a)		= do	(available, dict)	<- get
				case L.lookup a dict of
					Nothing	-> do	let chosen	= head available
							put (tail available, (a,chosen):dict)
							return $ RFree chosen
					(Just a') -> return $ RFree a'
nt t@(RNormal _ _)	= return t



instance Show Kind where
	show Kind	= "*"
	show k		= sk k

sk	:: Kind -> String
sk Kind = "*"
sk k	= k & kindArgs |> show & intercal " ~> " & pars


normalKind	:: Kind -> Bool
normalKind Kind	= True
normalKind _	= False

numberOfKindArgs	:: Kind -> Int
numberOfKindArgs Kind	= 0
numberOfKindArgs (KindCurry _ k)
			= 1 + numberOfKindArgs k


kindArgs	:: Kind -> [Kind]
kindArgs Kind	= [Kind]
kindArgs (KindCurry h t)
		= h:kindArgs t


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


traverseRTM	:: Monad m => (RType -> m RType) -> RType -> m RType
traverseRTM f (RApplied bt t)
		= do	bt'	<- traverseRTM f bt
			t'	<- traverseRTM f t
			RApplied bt' t' & return
traverseRTM f (RCurry at rt)
		= do	at'	<- traverseRTM f at
			rt'	<- traverseRTM f rt
			RCurry at' rt' & return
traverseRTM f t	= f t



onFree	:: (String -> RType) -> RType -> RType
onFree f (RFree a)
	= f a
onFree _ rt
	= rt


onFreeM	:: Monad m => (String -> m RType) -> RType -> m RType
onFreeM f (RFree a)
	= f a
onFreeM _ rt
	= return rt

foldRT	:: (RType -> a) -> ([a] -> a) -> RType -> a
foldRT f conct (RApplied bt t)
		= conct [foldRT f conct bt, foldRT f conct t]
foldRT f conct (RCurry at rt)
		= conct [foldRT f conct at, foldRT f conct rt]
foldRT f _ t	= f t


subs	:: [(Name, RType)] -> RType -> Exc RType
subs dict rt
	= do	let usedFrees	= freesInRT rt & nub			-- frees in the type
		let subsFrees	= dict |> snd >>= freesInRT		-- frees which might actually appear
		let subsFrees'	= subsFrees L.\\ (dict |> fst)	-- frees which might actually appear and are not substituted away
		let overlap	= dubbles (usedFrees ++ nub subsFrees')
		assert (L.null overlap) ("The substitution overlaps, target types contain free type variables which are in the source type too: "++commas overlap)
		let fetch free	= L.lookup free dict & fromMaybe (RFree free)
		traverseRT (onFree fetch) rt & return

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


applyTypeArgs	:: RType -> [RType] -> RType
applyTypeArgs t []	=  t
applyTypeArgs t tps	= L.foldl RApplied t tps

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
defaultFreeNames	= defaultFreeNames' "a"


defaultFreeNames'	:: String -> [Name]
defaultFreeNames' nm	= [0..] |> show |> (nm++)
