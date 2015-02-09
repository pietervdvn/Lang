module Languate.TypeTable.Bind.Bind where

{--
This module implements a simple bind, which works on Full Super Type Tables
--}

import StdDef

import Languate.Graphs.DirectedGraph

import Data.Map hiding (foldr)
import qualified Data.Map as M
import Data.Set hiding (foldr)
import qualified Data.Set as S
import qualified Data.List as L
import Data.Tuple
import Data.Maybe
import Prelude hiding (fail, lookup)

import StateT

import Languate.TAST
import Languate.TypeTable
import Languate.TypeTable.Bind.Binding

import Control.Monad hiding (fail)
import Control.Monad.Trans

{-
Binds t0 against t1. Gives a binding or a error message if the binding failed.

-}
bind	:: TypeTable -> Map Name [RType] -> RType -> RType ->
		Either String Binding
bind tt reqs t0 t1
	= do	let ctx	= Context reqs tt noBinding
		runstateT (b t0 t1) ctx |> snd |> binding


data Context	= Context 	{ frees		:: Map Name [RType]	-- keeps track of the supertypes (= requirements) for a given free. All bound frees should be included.
				, typeT 	:: TypeTable
				, binding 	:: Binding
				}

-- the monad we'll work with
type StMsg a	= StateT Context (Either String) a


b	:: RType -> RType -> StMsg ()
b t0 (RFree a)
	= do	addBinding (a, t0)
		conds	<- requirementsOn a
		mapM_ (b t0) conds
b (RFree a) t1
	= do	supers	<- requirementsOn a
		-- Try out all super types, if one matches, we're fine
		matches	<- mapM (\t -> catch False (b t t1 >> return True)) supers
		assert (or matches) $ "Could not bind the free '"++a++"' against "++show t1++" as it does not have the right super types"

b (RCurry t0 t1) (RCurry t0' t1')
	= do	b t0 t0'
		b t1 t1'
b t0@(RNormal fqn nm) t1
 | t0 == t1	= return ()
 | otherwise
	= do	stpReqs	<- fstt (fqn, nm) |> lookup t1
		let errMsg	= "Could not bind "++show t0++" against "++show t1++", no suitable super type found."
		assert (isJust stpReqs) errMsg
b t0@(RApplied bt at) t1@(RApplied bt' at')
	= do	-- First, try binding to special values
		try (bapp t0 t1) $ do
		-- then: try recursive binding
		b bt bt'
		b at at'
b t0@(RApplied bt at) t1
	= bapp t0 t1
b t0 t1	= fail $ "Could not bind "++show t0++" in "++show t1




{-

Bind applied is quite special.
It tries to bind t0 in t1 while using only the "allSupers" and "spareSuperTypes".

E.g.

bapp "List (Nat, Int)" "Dict k0 v0"

getBaseTid "Dict k0 v0" -> Dict
spareSuperTypes "Dict" (for List) -> [Dict k v, Dict k (List v)]
-- we see multiple dicts are possible.
-- We bind all against t1
mapM (\possibleSuper -> b t1 possSuper) [Dict k v, Dict k (List v)]
	-> [{k --> k0, v --> v0}, {k --> k0, v --> List v0}]
We see both are possible, this is a possible ambiguity.
-- TODO ^

On both lay requirements:
[a:(k,v), a:(k,v)]

The type argument is bound against these:

mapM (b "(Nat, Int)") [a:(k,v), a:(k,v)]
= [{k --> Nat, v --> Int}, {k --> Nat, v --> Nat}]


We now have two bindings:
Intermediate -> end result/supertype
[{k --> k0,  v --> v0}, {k --> k0, v --> List v0}]
Base type -> Intermediate
[{k --> Nat, v --> Int}, {k --> Nat, v --> Nat}]

We merge both bindings, by binding binding2 into binding1
= [{k0 --> Nat, v0 --> Int}, {k0 --> Nat, v0 --> FAIL}]





-- Same excercise, but with "Dict k0 [v0]" --

bapp "List (Nat,Int)" "Dict k0 [v0]"
getBaseTid "Dict k0 [v0]" -> Dict
spareSuperTypes "Dict" (for List) -> [Dict k v, Dict [v]]
mapM (\possSuper -> b t1="Dict k0 [v0]" possSuper) [Dict k v, Dict k [v]]
	-> [{k --> k0, v --> [v0]}, {k --> k0, v --> v0}]
reqs: [a:(k,v), a:(k,v)]
b at="(Nat, Int)" "a:(k,v)"
	-> [{k --> Nat, v --> Int}, {k --> Nat, v --> Int}]
merge:
[{k0 --> Nat, v0 --> FAIL}, {k0 --> Nat, v0 --> Int}]
binding succeeded!




-- Binding [[Nat]] against (a0*)*
bapp "List (List Nat)" "Collection (Collection a0)"
getBaseTID "Coll (Coll a0)" -> Coll
sst "Coll" (for List) -> [Collection a]
b t1="(a0*)*" possSuper="a*")-- should be mapm + list, but only one supertype
	 -> [{a --> a0*}]
reqs: [a:.]
b "List Nat" "a"
	-> [{a --> List Nat}]

merge bindings:
bind "List Nat" "a0*"
	=> {a0 --> Nat}
Binding succeeded!
-}


-- Bind applied. Special code which tries to find a supertype of a applied
bapp	:: RType -> RType -> StMsg ()
bapp t0 t1
	= fail "TODO"






-----------
-- UTILS --
-----------


requirementsOn	:: Name -> StMsg [RType]
requirementsOn a
	= get' frees |> findWithDefault [] a


fstt	:: TypeID -> StMsg FullSuperTypeTable
fstt tid
	= do	mFstt	<- get' typeT |> allSupertypes |> lookup tid
		assert (isJust mFstt) $ "No full super type table found for "++show tid
		return $ fromJust mFstt


addBinding	:: (Name, RType) -> StMsg ()
addBinding (n,t)
	= do	ctx	<- get
		let (Binding b)	= binding ctx
		-- check wether or not a conflicting binding exists
		let previous	= M.lookup n b
		assert (isNothing previous || t == fromJust previous) $
			"Conflicting bindings for '"++n++"' are found."++
			" It could be both bound to "++show (fromJust previous)++" and "++show t
		put $ ctx {binding = Binding $ M.insert n t b}

addFrees	:: [Name] -> StMsg ()
addFrees bound
	= do	ctx	<- get
		let frees'	= foldr (\n -> M.insert n []) (frees ctx) bound
		put $ ctx {frees = frees'}


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


try		:: StMsg a -> StMsg a -> StMsg a
try first backup
	= do	backup'	<- backup
		catch backup' first

-- Fails if requirements on the next type are returned. Assumes m return [], fails otherwise
noReqs	:: StMsg [a] -> StMsg ()
noReqs m
	= do	reqs	<- m
		assert (L.null reqs) "No requirements should be returned."


instance Show Context where
	show (Context frees _ b)= "Context "++sd frees ++ ", "++show b
