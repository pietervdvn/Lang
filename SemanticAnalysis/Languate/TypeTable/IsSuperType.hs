module Languate.TypeTable.IsSuperType where

{--
Implementation of the supertype check.
--}


import StdDef
import Languate.TypeTable
import Languate.TAST
import Normalizable
import Data.Map (Map, findWithDefault)
import Data.Set (Set, empty, member)
import qualified Data.Set as S
import Data.Map (Map, fromList, toList, keys, lookup)
import qualified Data.Map as M
import Prelude hiding (lookup)
import Data.Maybe
import Data.Either
import Data.List (nub)

import Control.Monad.Trans
import Control.Arrow
import StateT

import Debug.Trace

data Context	= Context 	{ frees		:: Map Name [RType]	-- keeps track of the supertypes (= requirements) for a given free. All bound frees should be included.
				, typeT 	:: TypeTable
				, binding 	:: Binding}
data Binding	= Binding (Map Name RType)	-- data type, and not a type alias to allow a custom show function

type StMsg a	= StateT Context (Either String) a


{- Tries to bind t0 in t1.

bind "A" "a"	= {a --> A}
bind "A" "B"	= {} if B is a supertype of A	== if A has a superType B
			failure otherwise
bind "Curry X Y" "a -> b"
		= {a --> X, b --> Y}

Important: the requirement-table should contain **each bound free**, to prevent double binding.

-}

bind	:: TypeTable -> Map Name [RType] -> RType -> RType -> Either String Binding
bind tt reqs
	= bind' (Context reqs tt noBinding)


bind' ctx t0 t1
	= runstateT (b' t0 t1) ctx |> snd |> binding

b'	:: RType -> RType -> StMsg ()
b' t0 t1
	= let (t0', t1')	= (normalize t0, normalize t1) in
		if t0' == t1' then return ()
			else b t0' t1'

b	:: RType -> RType -> StMsg ()
b t (RFree a)
	= do	reqs	<- requirementsOn a
		ctx	<- get
		allSupers	<- allSuperTypesOf t
		let unmetReqs	= filter (`notElem` allSupers) reqs
		if null unmetReqs then addBinding (a, t) else do
		failed $ "Could not bind "++show t++" to "++a++", type requirement is not met: "++show unmetReqs
b t0 t1@(RNormal _ _)
	= do	supers0	<- allSuperTypesOf' t0
		if t1 `elem` supers0 then return ()
		else failed $ "Could not bind "++show t0++" against "++show t1



superTypesOf	:: RType -> StMsg [RType]
superTypesOf (RNormal fqn nm)
	= fetchSTF (fqn, nm)  |> findWithDefault S.empty [] |> (S.map fst) |> S.toList
superTypesOf (RFree a)
	= requirementsOn a
superTypesOf (RApplied t args)
	= do	baseSupers	<- allSuperTypesOf' (RApplied t $ tail' args)
		argSupers	<- mapM allSuperTypesOf' args -- e.g. [[a,Eq,Ord], [b]]
		let mixedSupers	= perms $ baseSupers:argSupers-- e.g. [[a,b], [Eq, b], [Ord, b]]
		mapM appliedSuperTypes mixedSupers |> concat





{-
e.g.
"Collection" "a" -> Monoid, Eq (if a is Eq)
"Collection" "Eq"

-}
appliedSuperTypes	:: [RType] -> StMsg [RType]
appliedSuperTypes (base:args)
	= appSuper base args



-- Given the base type and arguments, will try to give each super type
appSuper	:: RType -> [RType] -> StMsg [RType]
appSuper t@(RNormal fqn nm) args
	= do	sttf	<- fetchSTF (fqn, nm)
		ctx		<- get
		-- kys = all free variables with exactly enough applied
		let kys	=  filter ((==) (length args) . length) $ keys sttf
		-- list of: (appliedFrees "a" "b", [(superType, ifTheseReqsAreMet)])
		let supers	= map (\k -> (k,S.toList $ findWithDefault (error "Huh?") k sttf)) kys
		let supers'	= mapMaybe (validateArgs ctx args) $ unmerge supers
		return $ trace (show t ++ show args ++ "--> "++show supers') supers'


-- Returns the supertype if the requirements (of the args) are met
validateArgs	:: Context -> [RType] -> ([Name], (RType, Map Name [RType])) -> Maybe RType
validateArgs ctx args (freeNames, (superT, reqs))
	= if validateKeys ctx args freeNames  reqs
		then Just superT
		else Nothing

{-
The supertypetable says:
if a is Eq, then Collection a is Eq
This checks wether [Int] [a] {a --> Eq} is fullfilled, thus if Int is a Eq
-}
validateKeys	:: Context -> [RType] -> [Name] -> Map Name [RType] -> Bool
validateKeys ctx args names reqs
	= let	reqsFor a	= findWithDefault [] a reqs
		namedArgs	= zip args names
		argsWithReqs	= map (second reqsFor) namedArgs in	-- e.g. (Int, [Eq]), meaning: Int should be EQ
		all (uncurry $ validateReqs ctx) argsWithReqs



{- Validates a binding.
Given a type t and a set of types it should be, returns True if t fulfills the reqs.
-}
validateReqs	:: Context -> RType -> [RType] -> Bool
validateReqs ctx t reqs
		= let 	bindings	= map (bind' ctx t) reqs
			failedBindings	= length $ lefts bindings in
			failedBindings == 0

{- Substitutes all frees which name is already bound in a different contect. The list are these "used" frees.

e.g.

["a"] (List (a,b)) -> List (a0,b)
-}
substitute'	:: [Name] -> RType -> RType
substitute' nms	= substitute (fmap RFree $ buildBinding nms)

-- Builds a binding, which substitutes away "bound" frees. e.g. [a,b,a0] -> Binding {a -> a1, b -> b0, a0 -> a11}
buildBinding	:: [Name] -> Map Name Name
buildBinding []	= M.empty
buildBinding (a:as)
		= let	base	= buildBinding as
			bound	= M.keys base ++ M.elems base
			canditates	= filter (`notElem` bound) [ a ++ show i | i <- [1..]]
			replacement	= head canditates in
			M.insert a replacement base



{- Replaces all frees in the given rtype. Unknown types are ignored
 e.g. {"a" -> Nat, "b" -> Bool} (RApplied Tuple [RFree a, RFree b, RFree c]) -> RApplied Dict [Nat, Bool, RFree c].     -}
substitute	:: Map Name RType -> RType -> RType
substitute dict t
		= traverseRT (_substitute dict) t

_substitute	:: Map Name RType -> RType -> RType
_substitute dict (RFree a)
		= findWithDefault (RFree a) a dict
_substitute _ t	= t


-- ## UTils
allSuperTypesOf' (RApplied t [])
			= allSuperTypesOf' t
allSuperTypesOf' t	= allSuperTypesOf t |> (t:) |> nub

allSuperTypesOf	:: RType -> StMsg [RType]
allSuperTypesOf t
	= do	supers	<- superTypesOf t -- TODO fix leak! Supertype could be "List a", where "a" suddenly got captured!
		supers'	<- mapM allSuperTypesOf supers |> concat
		return $ supers ++ supers'





fetchSTF	:: TypeID -> StMsg SuperTypeTableFor
fetchSTF tid
	= do	let err	= error $ "No supertype table found for "++show tid
		let f	= findWithDefault err tid
		get' (f . supertypes . typeT)


requirementsOn	:: Name -> StMsg [RType]
requirementsOn a
	= get' frees |> findWithDefault [] a


fetch		:: (Ord k) => k -> Map k (Set v) -> Set v
fetch		=  findWithDefault empty

addBinding	:: (Name, RType) -> StMsg ()
addBinding (n,t)
	= do	ctx	<- get
		let (Binding b)	= binding ctx
		put $ ctx {binding = Binding $ M.insert n t b}

perms	:: [[a]] -> [[a]]
perms []	= []
perms [ls]	= [[l] | l <- ls]
perms (ls:lss)
	= do	l	<- ls
		map (l:) $ perms lss

instance Show Binding where
	show	= sb

sb (Binding b)
	= sd b

instance Show Context where
	show (Context frees _ b)= "Context "++sd frees ++ ", "++show b

sd	:: (Show k, Show v) => Map k v -> String
sd  d
	= "{"++unwords (map (\(k, v) -> show k ++" --> "++show v) $ toList d)++"}"

noBinding	= Binding M.empty

failed str	= lift $ Left str
