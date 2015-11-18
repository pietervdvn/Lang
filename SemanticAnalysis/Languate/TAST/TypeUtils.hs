module Languate.TAST.TypeUtils where

{- Some usefull functions to manipulate rtypes -}

import StdDef
import Exceptions
import Languate.CheckUtils
import Languate.MarkUp as Mu
import HumanUtils
import Languate.FQN
import Languate.TAST.DefType
import Languate.TAST.Defaults
import Normalizable

import Control.Arrow

import Data.List as L
import Data.Map as M
import Data.Maybe
import State



------------------ GET, IS,... --------

getBaseTID	:: RType -> Maybe (FQN, Name)
getBaseTID (RNormal fqn nm)
		= Just (fqn, nm)
getBaseTID (RApplied bt _)
		= getBaseTID bt
getBaseTID _	= Nothing


------------- IS rfree, applied, ...

-- Means a base tid exists
isNormal	:: RType -> Bool
isNormal 	= isJust . getBaseTID


isRFree		:: RType -> Bool
isRFree (RFree _)
		= True
isRFree _	= False

isApplied	:: RType -> Bool
isApplied (RApplied _ _)
		= True
isApplied _	= False

isConj		:: RType -> Bool
isConj (RConj _)= True
isConj _	= False


isConjFree	:: RType -> Bool
isConjFree (RConj _)
		=  False
isConjFree (RCurry a b)
		= isConjFree a && isConjFree b
isConjFree (RApplied a b)
		= isConjFree a && isConjFree b
isConjFree _	= True


rtopLevelConj	:: RType -> [RType]
rtopLevelConj (RConj tps)
		= tps
rtopLevelConj t	= [t]


applyTypeArgs	:: RType -> [RType] -> RType
applyTypeArgs t []	=  t
applyTypeArgs t tps	= L.foldl RApplied t tps

applyTypeArgsFree	:: RType -> [Name] -> RType
applyTypeArgsFree t frees
			= applyTypeArgs t (frees |> RFree)


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



---------------------- FOLDING ----------------

-- Do f on each of the leaves
traverseRT	:: (RType -> RType) -> RType -> RType
traverseRT f (RApplied bt t)
		= RApplied (traverseRT f bt) $ traverseRT f t
traverseRT f (RCurry at rt)
		= RCurry (traverseRT f at) $ traverseRT f rt
traverseRT f (RConj tps)
		= RConj (tps |> traverseRT f)
traverseRT f t	= f t

-- do f on each of the leaves
traverseRTM	:: Monad m => (RType -> m RType) -> RType -> m RType
traverseRTM f (RApplied bt t)
		= do	bt'	<- traverseRTM f bt
			t'	<- traverseRTM f t
			RApplied bt' t' & return
traverseRTM f (RCurry at rt)
		= do	at'	<- traverseRTM f at
			rt'	<- traverseRTM f rt
			RCurry at' rt' & return
traverseRTM f (RConj tps)
		= do	tps'	<- tps |+> traverseRTM f
			return $ RConj tps'
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


-- collect each of the leaves
foldRT	:: (RType -> a) -> ([a] -> a) -> RType -> a
foldRT f conct (RApplied bt t)
		= conct [foldRT f conct bt, foldRT f conct t]
foldRT f conct (RCurry at rt)
		= conct [foldRT f conct at, foldRT f conct rt]
foldRT f conct (RConj tps)
		= tps |> f & conct
foldRT f _ t	= f t




-------------- Free related

freesInRT	:: RType -> [Name]
freesInRT	=  nub . foldRT frees concat
			where 	frees	:: RType -> [Name]
				frees (RFree a)	= [a]
				frees _		= []

freesInReq	:: (Name, [RType]) -> [Name]
freesInReq (nm, tps)
		= nm:(tps >>= freesInRT) & nub



-- Given "T x y z", gives a binding {a0 --> x, a1 --> y, a2 --> z}. Note: a0, a1, ... is hardcoded (defaultFreeNames)
canonicalBinding	:: RType -> Map Name RType
canonicalBinding t
	= appliedTypes t & zip defaultFreeNames & fromList


subs	:: [(Name, RType)] -> RType -> Exc RType
subs dict rt
	= do	let usedFrees	= freesInRT rt & nub			-- frees in the type
		let subsFrees	= dict |> snd >>= freesInRT		-- frees which might actually appear
		let subsFrees'	= subsFrees L.\\ (dict |> fst)	-- frees which might actually appear and are not substituted away
		let overlap	= dubbles (usedFrees ++ nub subsFrees')
		assert (L.null overlap) ("The substitution overlaps, target types contain free type variables which are in the source type too: "++commas overlap)
		let fetch free	= L.lookup free dict & fromMaybe (RFree free)
		traverseRT (onFree fetch) rt & return


subsReq	:: [(Name, Name)] -> RTypeReq -> Exc RTypeReq
subsReq dict reqs
	= do	let subsName a	= fromMaybe a $ L.lookup a dict
		let dict'	= dict ||>> RFree
		(reqs |> first subsName) |+> onSecond (|+> subs dict')


------------------ NORMALIZING ---------------


instance Normalizable RType where
	normalize rt	= normalizeRT "a" rt & fst

normalizeRT names rt	= runstate (nt rt) (defaultFreeNames' names, []) |> snd

nt	:: RType -> State ([Name],[(Name, Name)]) RType
nt (RApplied t0 t1)	= do	t0'	<- nt t0
				t1'	<- nt t1
				return $ RApplied t0' t1'
nt (RCurry t0 t1)	= do	t0'	<- nt t0
				t1'	<- nt t1
				return $ RCurry t0' t1'
nt (RConj [t])		= nt t
nt (RConj tps)		= tps |+> nt |> RConj
nt (RFree a)		= do	(available, dict)	<- get
				case L.lookup a dict of
					Nothing	-> do	let chosen	= head available
							put (tail available, (a,chosen):dict)
							return $ RFree chosen
					(Just a') -> return $ RFree a'
nt t@(RNormal _ _)	= return t




------------------ SHOW RELATED -----------------

instance Show RType where
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
st short (RConj tps)
		= pars (tps |> st short & intercal " & ")

showCommaSep short t0@(RApplied bt at)
	= st short bt ++ " "++st short at
showCommaSep short t
	= st short t

showRTypeReq	:: RTypeReq -> String
showRTypeReq nreqs
	= let 	showReq (name, rtypes)
			= name++": "++(rtypes |> show & commas) in
		nreqs |> showReq |> pars & commas

showRTypeReq'	:: (Name, [RType]) -> String
showRTypeReq' (nm, subs)
		=  nm ++":" ++ intercalate ", " (fmap (st True) subs)



instance Show Signature where
	show 	= ssign

ssign	:: Signature -> String
ssign sign
	= show (signFQN sign) ++ "." ++ signName sign ++ ": " ++ show (signTypes sign)
