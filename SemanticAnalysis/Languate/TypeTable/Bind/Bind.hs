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

import Control.Monad.Trans
import StateT

import Languate.TAST
import Languate.TypeTable
import Languate.TypeTable.Bind.Binding

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
b	= todo














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
	= let   ctx	= Context reqs tt noBinding
		t'	= normalize t in
		runstateT (superTypesOf' [] t') ctx |> fst


{-
Gives all the supertypes of the given type.
All frees which appear in context.frees.keys will be substituted out, to prevent false capturing.

The extra set passed are supertypes which are already seen, to prevent loops.

Sometimes, filled in frees cause a extra supertype to exist. E.g:
MyList		is List
List Char	is Showable
=> MyList Char  is Showable
This means a extra binding can be passed.

This function recursively expands the supertypes and delegates the heavy work to _sto
-}
superTypesOf'	:: [RType] -> RType -> Set RType -> StMsg (Set RType)
superTypesOf' binding t seen
	= do	supers	<- _sto binding t seen
		superSupers	<- mapM (_sto binding $ union seen supers) (S.toList $ supers \\ seen) |> unions
		return $ union supers superSupers


_sto	:: [RType] -> RType -> StMsg (Set RType)
_sto	= todo





















-----------
-- UTILS --
-----------

fetchSTTF	:: TypeID -> StMsg SuperTypeTableFor
fetchSTTF tid
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


failed str	= lift $ Left str


instance Show Context where
	show (Context frees _ b)= "Context "++sd frees ++ ", "++show b
