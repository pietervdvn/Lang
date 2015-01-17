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

import Control.Monad.Reader

{- Returns LT if type1 is a subtype of type2, GT in the opposite case and EQ if both are equals.
When no ordering is possible, nothing is returned.

We assume both the types live in a context where free type variables are bounded against certain super types. These are passed along.
We also assume both types have the same kind. If not, ordering is impredictable.

t0 supertype of t1 if:
RNormal blabla > RNormal _
RFree x > RFree y <=> x has a type constraint which is a supertype of y
RApplied x y > RApplied a b
	<=> x > a && y >= b || x y > y b
	e.g. 	Collection a < Mappable a
		(a:Show)* < Mappable a
		a*	? Mappable (a:Show)
x -> y > a -> b
	<=> x > a && y >= b
		Nat -> Nat > Nat' -> Nat'
		Nat' -> Nat > Nat' -> Nat'
		Nat -> Nat' > Nat' -> Nat'
	This follows out of the 'Curry a b = a -> b' equivalence

RNormal x > Free
	<=> Free has a type requirement which is x or a subtype of x.
	Thus: "Bool" > "a:Bool"
RNormal x > RApplied a b
	<=> a, applied to the free variables, is a subtype of x

Whenever loops are found, the types are treated as equals:
cat A
cat B
instance A is B
instance B is A

-}
isSupertypeOf	:: TypeTable -> Map Name [RType] -> RType -> RType -> Maybe Ordering
isSupertypeOf tt frees t1 t2
	= runReaderT (isto (normalize t1) (normalize t2)) $ Context frees tt


data Context	= Context {frees :: Map Name [RType], typeT :: TypeTable}

supersOf free ctx
	= findWithDefault [anyType] free $ frees ctx


--
isto	:: RType -> RType -> ReaderT Context Maybe Ordering
isto t0@(RNormal fqn0 nm0) t1@(RNormal fqn1 nm1)
	= do	let tid0	= (fqn0, nm0)
		let tid1	= (fqn1, nm1)
		if tid0 == tid1 then return EQ else do
		-- We do not know about applied frees here, so [] as key.
		-- No frees => No reqs on the frees => We only care about FST
		supers0	<- fetchSTF tid0 |> fetch [] |> (S.map fst)
		supers1	<- fetchSTF tid1 |> fetch [] |> (S.map fst)
		let t0inT1	= t0 `member` supers1
		let t1inT0	= t1 `member` supers0
		if t0inT1 && t1inT0 then return EQ else do
		if t0inT1 then return GT else do
		if t1inT0 then return LT else do
		lift Nothing


fetchSTF	:: TypeID -> ReaderT Context Maybe SuperTypeTableFor
fetchSTF tid
	= do	let err	= error $ "No supertype table found for "++show tid
		let f	= findWithDefault err tid
		asks (f . supertypes . typeT)


fetch		:: (Ord k) => k -> Map k (Set v) -> Set v
fetch		=  findWithDefault empty
