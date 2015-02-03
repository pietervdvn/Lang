module Languate.TypeTable.Bind.Bind (superTypesOf, bind) where

{--
This module implements both bind and superTypesOf. Should be different modules, but ghc does not allow cyclic imports :(
--}

import StdDef
import Normalizable

import Data.Set (Set, empty, union, unions, (\\))
import qualified Data.Set as S
import Data.Map (Map, findWithDefault)
import qualified Data.Map as M
import Prelude hiding (fail)

import Control.Monad.Trans
import StateT
import MarkDown

import Languate.TAST
import Languate.TypeTable
import Languate.TypeTable.Bind.Binding
import Languate.TypeTable.Bind.Substitute

data Context	= Context 	{ frees		:: Map Name [RType]	-- keeps track of the supertypes (= requirements) for a given free. All bound frees should be included.
				, typeT 	:: TypeTable
				, binding 	:: Binding}

-- our monad we'll work with
type StMsg a	= StateT Context (Either String) a

----------
-- BIND --
----------

{- Tries to bind t0 in t1. Gives the resulting binding (or a err msg).

bind "A" "a"	= {a --> A}
bind "A" "B"	= {} if A has B as a supertype
		  failure otherwise
bind "Curry X Y" "a -> b"
		= {a --> X, b --> Y}

Important: the requirement-table should contain **each bound free**, to prevent double binding.
-}

bind	:: TypeTable -> Map Name [RType] -> RType -> RType -> Either String Binding
bind tt reqs t0 t1
	= let   ctx	= Context reqs tt noBinding
		[t0', t1']	= [t0,t1] |> normalize in
		runstateT (bind' t0' t1') ctx |> snd |> binding

-- if both are the same, early return; if not delegate to b
bind'	:: RType -> RType -> StMsg ()
bind' t0 t1
 | t0 == t1	= return ()
 | otherwise	= b t0 t1


{-
The actual heavy lifting
-}
b	:: RType -> RType -> StMsg ()
b t0 (RFree a)
	= do	validateRequirement t0 a
		addBinding (a,t0)
b t0 t1
	= do	t0Supers	<- superTypesOf' [] t0
		if t1 `S.member` t0Supers then return ()
			else fail $ "Could not bind "++show t0++" against "++ show t1













----------------
-- SUPERTYPES --
----------------


{-
Gives all the supertypes of the given rtype.
Bindings might have happened under the hood, but are not given.

Same as superTypesOf', but not within the monad
-}
superTypesOf	:: TypeTable -> RType -> Either String (Set RType)
superTypesOf tt t
	= let   ctx	= Context M.empty tt noBinding
		t'	= normalize t in
		runstateT (superTypesOf' [] t') ctx |> fst


{-
Gives all the supertypes of the given type.
All frees which appear in context.frees.keys will be substituted out, to prevent false capturing.

Sometimes, filled in frees cause a extra supertype to exist. E.g:
MyList		is List
List Char	is Showable
=> MyList Char  is Showable
This means a extra binding can be carried out, but these are not given.

-}
superTypesOf'	:: [RType] -> RType -> StMsg (Set RType)
superTypesOf' binding t
		= _superTypesOf' binding t S.empty



-- This function recursively expands the supertypes and delegates the heavy work to _sto; The extra set passed are supertypes which are already seen, to prevent loops.
_superTypesOf'	:: [RType] -> RType -> Set RType -> StMsg (Set RType)
_superTypesOf' binding t seen
	= do	bound	<- get' frees |> M.keys	-- type variables which are used
		supers	<- _sto binding t	-- direct super types, with newly in substituted values
		let supersOf t	= _superTypesOf' binding t (union seen supers)
		let toGetSupers	= S.toList $ supers \\ seen
		indirectSupers	<- mapM supersOf toGetSupers |> unions
		return $ union supers indirectSupers

{- Same as _sto, but where all frees -which might get captured- are substituted out.
We do not care about eventual extra requirements (e.g. Set a:Eq), as these are propageted elsewhere
-}
_sto'	:: [RType] -> RType -> StMsg (Set RType)
_sto' b t
	= do 	supers	<- _sto' b t |> S.toList
		supers'	<- mapM safeSubstitute supers
		return $ S.fromList supers'
safeSubstitute t
	= do	bound	<- get' frees |> M.keys
		let (t', used)	= substitute' bound t
		addFrees used
		return t'
{-
Gets the direct super types of the given type.

The extra binding is to pass through bound frees.

E.g.

MyList Char
MyList is List
=> We should check for supertypes of List Char too, and substitute out the first free of List.
-}
_sto	:: [RType] -> RType -> StMsg (Set RType)
_sto binding (RNormal fqn nm)
	= fetchSTTF (fqn, nm)  |> findWithDefault S.empty [] |> (S.map fst)
_sto binding (RFree a)
	= requirementsOn a
_sto binding (RApplied bt at)
	= do	sttf	<- fetchSTTF bt
		todos "Pickup here!" -- TODO Pickup point



















----------------
-- VALIDATION --
----------------

{-
Validates that the given type can be used to be bound in the given Free, seen its reqs.

e.g.
a should be a Eq.
This means that Any can not be bound to a, but e.g. Int can.
-}
validateRequirement	:: RType -> Name -> StMsg ()
validateRequirement t a
	= do	neededSupers	<- requirementsOn a
		actualSupers	<- superTypesOf' [] t
		let missing	= neededSupers \\ actualSupers
		if S.null missing then return ()
			else fail $ "The type "++show t++" can not be bound to the free type variable '"++a++"', as it can not fullfill the " ++ plural (S.size missing) "type requirement" ++ show missing





-----------
-- UTILS --
-----------

fetchSTTF	:: RType -> StMsg SuperTypeTableFor
fetchSTTF (RApplied bt _)
	= fetchSTTF bt
fetchSTTF (RNormal fqn nm)
	= do	let tid	= (fqn, nm)
		let err	= error $ "No supertype table found for "++show tid
		let f	= findWithDefault err tid
		get' (f . supertypes . typeT)


requirementsOn	:: Name -> StMsg (Set RType)
requirementsOn a
	= get' frees |> findWithDefault [] a |> S.fromList


fetch		:: (Ord k) => k -> Map k (Set v) -> Set v
fetch		=  findWithDefault empty

addBinding	:: (Name, RType) -> StMsg ()
addBinding (n,t)
	= do	ctx	<- get
		let (Binding b)	= binding ctx
		put $ ctx {binding = Binding $ M.insert n t b}

addFrees	:: [Name] -> StMsg ()
addFrees bound
	= do	ctx	<- get
		let frees'	= foldr (\n -> M.insert n []) (frees ctx) bound
		put $ ctx {frees = frees'}


fail str	= lift $ Left str

catch		:: a -> StMsg a -> StMsg a
catch a m	=  do	ctx	<- get
			let mRes	= runstateT m ctx
			case mRes of
				Left _	-> return a
				Right (a, ctx')	-> do	put ctx
							return a


instance Show Context where
	show (Context frees _ b)= "Context "++sd frees ++ ", "++show b
