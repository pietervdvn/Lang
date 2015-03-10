module Languate.TypeTable.Bind.Substitute (substitute, substitute', buildBinding, concatBindings, substituents, mergeBinding, asBinding, unbind) where

{--
This module implements substitute and friends
--}
import StdDef
import Languate.TypeTable
import Data.Map hiding (filter)
import qualified Data.Map as M

import Languate.TAST


-- execute substitution of b1 everywhere in b0. e.g. concatBindings {a --> b, x --> y} {b --> c} = {a --> c}
concatBindings	:: Binding -> Binding -> Binding
concatBindings (Binding dict) b1
	= dict |> substitute b1 & Binding


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
The list 'nms' represents thos frees which are already bound
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


-- Builds a binding, which substitutes away "bound" frees. e.g. [a,b,a0] -> Binding {a -> a1, b -> b0, a0 -> a11}
buildBinding	= Binding . M.map RFree . _buildBinding

-- Builds a binding, which substitutes away "bound" frees. e.g. [a,b,a0] -> Binding {a -> a1, b -> b0, a0 -> a11}
_buildBinding	:: [Name] -> Map Name Name
_buildBinding []	= empty
_buildBinding (a:as)
		= let	base	= _buildBinding as
			bound	= keys base ++ elems base
			canditates	= filter (`notElem` bound) [ a ++ show i | i <- [1..]]
			replacement	= head canditates in
			insert a replacement base
