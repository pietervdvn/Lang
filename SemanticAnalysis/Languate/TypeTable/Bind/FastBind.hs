module Languate.TypeTable.Bind.FastBind (bind) where

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
b t0@(RNormal fqn nm) t1@(RNormal _ _ )
	= do	directSupers	<- supersOf (fqn,nm)
		assert (t1 `S.member` directSupers) $ "Could not bind "++show t0++" in "++show t1
		return ()
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













-----------
-- UTILS --
-----------



requirementsOn'	:: Name -> StMsg (Set RType)
requirementsOn' a
	= requirementsOn a |> S.fromList



requirementsOn	:: Name -> StMsg [RType]
requirementsOn a
	= get' frees |> findWithDefault [] a


supersOf	:: TypeID -> StMsg (Set RType)
supersOf tid
	= do	sttf'	<- get' typeT |> supertypes |> M.lookup tid
		assert (isJust sttf') $ "No super type table found for "++show tid
		let (Just sttf)	= sttf'
		let found	= S.map fst $ findWithDefault S.empty [] sttf
		return found



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
