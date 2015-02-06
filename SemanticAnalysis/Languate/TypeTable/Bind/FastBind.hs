module Languate.TypeTable.Bind.FastBind (superTypesOf, bind) where

{--
This module implements both bind and superTypesOf. Should be different modules, but ghc does not allow cyclic imports :(
--}

import StdDef
import Normalizable

import Data.Set (Set, empty, union, unions, (\\))
import qualified Data.Set as S
import Data.Map (Map, findWithDefault)
import qualified Data.Map as M
import Data.Tuple
import Data.Maybe
import Prelude hiding (fail)
import Control.Monad hiding (fail)
import Control.Monad.Trans
import StateT
import MarkDown hiding (when)

import Languate.TAST
import Languate.TypeTable
import Languate.TypeTable.Bind.Binding
import Languate.TypeTable.Bind.Substitute

import Debug.Trace

data Context	= Context 	{ frees		:: Map Name [RType]	-- keeps track of the supertypes (= requirements) for a given free. All bound frees should be included.
				, typeT 	:: TypeTable
				, binding 	:: Binding
				, knownDirectSuperTypes	-- cache of STO
						:: Map RType (Set RType)
				, knownAllSuperTypes	-- cache of superTypes
						:: Map RType (Set RType)
				}

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
	= let   ctx	= Context reqs tt noBinding M.empty M.empty
		[t0', t1']	= [t0,t1] |> normalize	in
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
b t0 t1@(RNormal fqn nm)
	= todos $ "Can we write "++show t0++" as a "++show t1++"?"
b t0 (RFree a)
	= do	supers	<- requirementsOn a
		mapM (bind' t0) supers
		addBinding (a,t0)
b t0@(RApplied bt at) t1@(RApplied bt' at')
	= do	bind' bt bt'
		bind' at at'
b (RCurry t0 t0') (RCurry t1 t1')
	= do	bind' t0  t1
		bind' t0' t1'
b t0@(RTuple tps0) t1@(RTuple tps1)
	| length tps0 /= length tps1
		= fail $ "Could not bind "++show t0++" and "++show t1++" as the tuples do not have the same length"
	| otherwise
		= mapM_ (uncurry b) $ zip tps0 tps1
b t0 t1		= fail $ "Could not bind "++show t0++" against "++ show t1













----------------
-- SUPERTYPES --
----------------


{-
Gives all the supertypes of the given rtype.
Bindings might have happened under the hood, but are not given.

Same as superTypesOf', but not within the monad
-}
superTypesOf	:: TypeTable -> Map Name [RType] -> RType -> Either String (Set RType)
superTypesOf tt reqs t
	= let   ctx	= Context reqs tt noBinding M.empty M.empty
		t'	= normalize t	in
		runstateT (superTypesOf' t') ctx |> fst


{-
Gives all the supertypes of the given type.

Sometimes, filled in frees cause a extra supertype to exist. E.g:
MyList		is List
List Char	is Showable
=> MyList Char  is Showable
This means a extra binding can be carried out, but these are not given.

-}
superTypesOf'	:: RType -> StMsg (Set RType)
superTypesOf' t
		= do	cache	<- get' knownAllSuperTypes
			if t `M.member` cache
				then return $ findWithDefault (error "Huh?") t cache
				else _superTypesOf' t S.empty



{-
This function recursively expands the supertypes and delegates the heavy work to _sto;
A 'already seen' set of types is passed recursively, to prevent infinite loops
-}
_superTypesOf'	:: RType -> Set RType -> StMsg (Set RType)
_superTypesOf' t seen
	= do	bound	<- get' frees |> M.keys	-- type variables which are used
		supers	<- _sto' t	-- direct super types, with newly in substituted values
		let supersOf t	= _superTypesOf' t (union seen supers)
		let toGetSupers	= supers `S.difference` seen
		indirectSupers	<- mapM supersOf (S.toList toGetSupers) |> unions
		let allSupers	= union supers indirectSupers
		ctx	<- get
		cache	<- get' knownAllSuperTypes
		put ctx { knownAllSuperTypes = M.insert t allSupers cache }
		return allSupers



_sto'	:: RType -> StMsg (Set RType)
_sto' rt
	= do	cache	<- get' knownDirectSuperTypes
		if rt `M.member` cache
			then return $ findWithDefault (error "Wut?") rt cache
			else do	got	<- _sto rt
				ctx	<- get
				put $ ctx {knownDirectSuperTypes = M.insert rt got cache}
				return got

{-
Gets the direct super types of the given type.

Normally, all frees in the returned type should be uncaptured.

E.g.

MyList Char
MyList is List
=> We should check for supertypes of List Char too, and substitute out the first free of List.
-}
_sto	:: RType -> StMsg (Set RType)
_sto t@(RNormal _ _)
	= fetchRSTTs t ||>> isA |> S.unions
_sto (RFree a)
	= requirementsOn a
_sto t@(RApplied bt at)
	= do	baseSupers	<- _sto' bt |> S.toList
		argSupers	<- _sto' at |> S.toList
		-- applied 'hybrid' supers
		let appBaseSupers	= [RApplied bt at' | at' <- argSupers]
		let appArgSupers	= [RApplied bt' at | bt' <- baseSupers]
		let appSupers'	= S.fromList ( appBaseSupers ++ appArgSupers)
		-- Instance supers, e.g. "Collection Eq" => Eq
		supers	<- fetchRSTTs t ||>> isA
		return $ S.unions (appSupers' : supers)
_sto (RCurry bt rt)
	= do	bSupers	<- _sto' bt |> S.toList
		rSupers	<- _sto' rt |> S.toList
		let possible	= S.fromList [RCurry bt' rt' | bt' <- bSupers, rt' <- rSupers]
		return $ S.insert anyType possible
_sto (RTuple tps)
	= do	tps'	<- mapM (\t -> _sto' t |> S.toList |> (t:)) tps
		-- all possible combinations of supertypes
		let combinations	= perms tps'
		let tpls	= combinations |> RTuple
		return tpls |> S.fromList |> S.insert anyType














-----------
-- UTILS --
-----------



requirementsOn	:: Name -> StMsg (Set RType)
requirementsOn a
	= get' frees |> findWithDefault [] a |> S.fromList


fetch		:: (Ord k) => k -> Map k (Set v) -> Set v
fetch		=  findWithDefault empty

addBinding	:: (Name, RType) -> StMsg ()
addBinding (n,t)
	= do	ctx	<- get
		let (Binding b)	= binding ctx
		-- check wether or not a conflicting binding exists
		let previous	= M.lookup n b
		assert (isNothing previous || t == fromJust previous) $ "Conflicting bindings for '"++n++"' are found. It could be both bound to "++show (fromJust previous)++" and "++show t
		put $ ctx {binding = Binding $ M.insert n t b}

addFrees	:: [Name] -> StMsg ()
addFrees bound
	= do	ctx	<- get
		let frees'	= foldr (\n -> M.insert n []) (frees ctx) bound
		put $ ctx {frees = frees'}


-- Returns true if t0 binds validly into t1. Catches failures
doesBind	:: RType -> RType -> StMsg Bool
doesBind t0 t1	= catch False (bind' t0 t1 >> return True)

isSubtypeOf	= doesBind
isSupertypeOf	= flip doesBind

fail		:: String -> StMsg a
fail		=  lift . Left


assert True _	= return ()
assert False msg	= fail msg


catch		:: a -> StMsg a -> StMsg a
catch backup stmsg
	= do	ctx	<- get
		case runstateT stmsg ctx of
			(Left _)	-> return backup
			(Right (a, ctx))	-> put ctx >> return a



instance Show Context where
	show (Context frees _ b _ _)= "Context "++sd frees ++ ", "++show b
