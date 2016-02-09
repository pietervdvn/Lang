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


dissassemble	:: RType -> (RType, [RType])
dissassemble (RApplied bt at)
	= let (b, args)	= dissassemble bt in
		(b, args++[at])
dissassemble t
	= (t, [])


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

{- removes an argument type from the curry chain
	Returns original type if no curry is detected
-}
dropCurry	:: RType -> RType
dropCurry (RCurry arg rt)
		= rt
dropCurry t	= t

dropCurry'	:: CType -> CType
dropCurry' (tp, reqs)
		= (tp & dropCurry, reqs)



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


freesInRT	= nub . freesInRT'


-- one occurance per time the free is occured
freesInRT'	:: RType -> [Name]
freesInRT'	=  foldRT frees concat
			where 	frees	:: RType -> [Name]
				frees (RFree a)	= [a]
				frees _		= []

freesInReq	:: (Name, [RType]) -> [Name]
freesInReq (nm, tps)
		= nub $ nm:(tps >>= freesInRT')

buildSafeSubs'	:: [Name] -> [(Name, Name)]
buildSafeSubs' used
	= let	new = defaultFreeNames & L.filter (`notElem` used) & take (length used)
		in zip used new

buildSafeSubs	:: FullRTypeReq -> [(Name, Name)]
buildSafeSubs (Reqs ls)
	= let	used	= ls |> fst
		in
		buildSafeSubs' used


-- renames all the free type variables to prevent name collisions.
buildSafeCType	:: FullRTypeReq -> CType -> Exc CType
buildSafeCType reqs ctype
	= do	let mapping	= buildSafeSubs reqs
		subsCType mapping ctype

-- Given "T x y z", gives a binding {a0 --> x, a1 --> y, a2 --> z}. Note: a0, a1, ... are hardcoded (defaultFreeNames)
canonicalBinding	:: RType -> Map Name RType
canonicalBinding t
	= appliedTypes t & zip defaultFreeNames & fromList

subs	= subs' True

subs'	:: Bool -> [(Name, RType)] -> RType -> Exc RType
subs' warn dict rt
	= inside ("In the substitution of "++show rt++" with "++show dict) $
	  do	let usedFrees	= freesInRT rt & nub			-- frees in the type
		let subsFrees	= dict |> snd >>= freesInRT		-- frees which might actually appear
		let subsFrees'	= subsFrees L.\\ (dict |> fst)	-- frees which might actually appear and are not substituted away
		let overlap	= dubbles (usedFrees ++ nub subsFrees')
		errIf (warn && not (L.null overlap)) ("The substitution overlaps, target types contain free type variables which are in the source type too: "++commas overlap)
		let fetch free	= L.lookup free dict & fromMaybe (RFree free)
		traverseRT (onFree fetch) rt & return


subsReq	:: [(Name, Name)] -> RTypeReq -> Exc RTypeReq
subsReq dict reqs
	= do	let subsName a	= fromMaybe a $ L.lookup a dict
		let dict'	= dict ||>> RFree
		(reqs |> first subsName) |+> onSecond (|+> subs dict')


subsCType	:: [(Name, Name)] -> CType-> Exc CType
subsCType sub (rTyp, reqs)
	= do	rTyp'	<- rTyp & subs (sub ||>> RFree)
		reqs'	<- subsReq sub reqs
		return (rTyp', reqs')



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



{- Normalizing the ctype is done by
	- removing all useless constraints (constraints on frees that do not exist)
	- removing all frees with a single occurance
	- renaming all frees. The first occured free will be renamed a0, the next one a1, ...


-}
normalizeCType	:: CType -> Exc CType
normalizeCType (rtp, reqs)
	= do	let reqs'	= _addReq [] (rtp & freesInRT) (reqs |> (id &&& freesInReq))
					& merge ||>> concat
		-- we don't need the name of the req, it's not a problem if it appears there
		let allFrees	= freesInRT' rtp ++ (reqs >>= snd >>= freesInRT)
		-- what free does appear only once?
		let uniquelyUsed	= nub allFrees L.\\ dubbles allFrees
		-- what requirements are on these? None might apply
		let (singleOccuranceReqs, restingReq)
				= L.partition ( (`elem` uniquelyUsed) . fst) reqs'
		let mapping	= singleOccuranceReqs ||>> RConj 	:: [(Name, RType)]
		rtp'		<- subs mapping rtp
		restingReq'	<- restingReq |+> onSecond (|+> subs mapping)	-- we substitute the values. Names cannot occur

		-- And as last, we normalize the free variables
		let origFrees	= nub (freesInRT rtp' ++ (restingReq' >>= freesInReq))
		let newFrees	= zip origFrees defaultFreeNames	:: [(Name, Name)]
		rtp''		<- subs (newFrees ||>> RFree) rtp'
		restingReq''	<- subsReq newFrees restingReq'
		return (rtp'', restingReq'')


_addReq		:: [Name] -> [Name] -> [( (Name, [RType]), [Name])] -> RTypeReq
_addReq _ [] _	= []
_addReq _ _ []	= []
_addReq seen usedFrees reqs
	= let	usedIn nms	= nms |> (`elem` usedFrees) & or
		(needed, unused)= reqs & L.partition (usedIn . snd)
		seen'		= usedFrees ++ seen
		newUsedFrees	= (needed >>= snd) L.\\ seen'
		recNeeded	= _addReq seen' newUsedFrees unused
		in
		(needed |> fst) ++ recNeeded


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
	= show (signFQN sign) ++ "." ++ signName sign ++ ": " ++ show (signType sign)
