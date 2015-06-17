module Languate.TypeTable.Bind.Substitute (substitute, substitute', buildBinding, buildBinding', concatBindings, substituents, mergeBinding, asBinding, unbind, substituteReq, substituteAway, chainBindings) where

{--
This module implements substitute and friends
--}
import StdDef
import Languate.TypeTable
import Data.Map hiding (filter)
import qualified Data.Map as M

import Languate.TAST
import Data.List (nub)


{- execute substitution of b1 everywhere in b0. e.g. concatBindings {a --> b, x --> y} {b --> c} = {a --> c, b --> c, x --> y}
Equivalent to first substituting b0, then b1
-}
concatBindings	:: Binding -> Binding -> Binding
concatBindings (Binding dict) b1
	= let 	dict'	= b1 & unbind & filterWithKey (\k _ -> k `notMember` dict) in	-- throw away "invisible" values
		dict |> substitute b1 & filterWithKey (\k v -> not $ isSame k v) & M.union dict' & Binding
		where isSame	a (RFree b)	= a == b
		      isSame	_ _		= False

{-
chainBindings {a --> b,x --> y} {b --> c, g --> h} = {a --> c, x --> y}
Drops starting links
-}
chainBindings	:: Binding -> Binding -> Binding
chainBindings (Binding dict) b1
	= let	dict'	= b1 & unbind & filterWithKey (\k _ -> k `notMember` dict) in	-- throw away "invisible" values
		dict |> substitute (Binding dict') & Binding

-- Gets variables which will be substituted
substituents	:: Binding -> [Name]
substituents (Binding dict)
		= keys dict

unbind	:: Binding -> Map Name RType
unbind (Binding dict)	= dict

-- Takes a union of the binding. Overlap will prefer the first binding
mergeBinding	:: Binding -> Binding -> Binding
mergeBinding (Binding dict0) (Binding dict1)
	= Binding $ M.union dict0 dict1


{- Substitutes all frees which name is already bound in a different context.
The list 'nms' represents those frees which are already bound
e.g.
["a"] (List (a,b)) -> List (a0,b)
-}
substitute'	:: [Name] -> RType -> (RType, [Name])
substitute' nms t
	= let 	binding@(Binding dict)	= buildBinding nms
		used	= M.keys dict in
		(substitute binding t, used)

asBinding		:: Map Name Name -> Binding
asBinding dict	= dict |> RFree & Binding

{- Replaces frees in the given rtype. Unknown types are ignored
 e.g. {"a" -> Nat, "b" -> Bool} (RApplied Tuple [RFree a, RFree b, RFree c]) -> RApplied Dict [Nat, Bool, RFree c].     -}
substitute	:: Binding -> RType -> RType
substitute (Binding dict)
		= traverseRT (_substitute dict)


_substitute	:: Map Name RType -> RType -> RType
_substitute dict (RFree a)
		= findWithDefault (RFree a) a dict
_substitute _ t	= t


-- given the used frees, substitutes given types + reqs so no clashes persist
substituteAway	:: [Name] -> ([RType], RTypeReqs) -> ([RType], RTypeReqs)
substituteAway frees (rtps, reqs)
	= let	frees' = nub $ frees ++  (rtps >>= freesInRT) ++ (reqs >>= freesInReq)
		binding	= buildBinding' frees'
		binding'	= asBinding binding in
		(rtps |> substitute binding',
			reqs |> substituteReq binding)

substituteReq	:: Map Name Name -> (Name, [RType]) -> (Name, [RType])
substituteReq dict (n, tps)
	= (findWithDefault n n dict, tps |> substitute (asBinding dict))

-- Builds a binding, which substitutes away "bound" frees. e.g. [a,b,a0] -> Binding {a -> a1, b -> b0, a0 -> a11}
buildBinding	= Binding . M.map RFree . buildBinding'

-- Builds a binding, which substitutes away "bound" frees. e.g. [a,b,a0] -> Binding {a -> a1, b -> b0, a0 -> a11}
buildBinding'	:: [Name] -> Map Name Name
buildBinding' []	= empty
buildBinding' (a:as)
		= let	base	= buildBinding' as
			bound	= keys base ++ elems base
			canditates	= filter (`notElem` bound) [ a ++ show i | i <- [1..]]
			replacement	= head canditates in
			insert a replacement base	
