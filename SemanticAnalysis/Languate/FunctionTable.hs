module Languate.FunctionTable where

{--

This module implements a table keeping track of all the functions

--}

import Data.Map
import Prelude hiding (lookup)
import StdDef
import Languate.TAST

{- A signature is a function identifier. When looking up a function with given signature, this function should be **all** of the given types (e.g. Assoc + Commut), and can be more.

Note that the different types the function carries will be orderable. The function will have a (meaningfull) upper type.

Prop X A & Commut X A		is X -> X -> A
Prop Y A & Commut Y A		is Y -> Y -> A


Int -> Int -> Int	& (Assoc Nat : Nat -> Nat -> Nat)
Nat -> Nat -> Nat	& (Assoc Nat : Nat -> Nat -> Nat)


-}
data Signature	= Signature Name [RType]
	deriving (Eq, Ord)

instance Show Signature where
	show (Signature n t)
	 	= tabs 2 n ++ ": "++ show t

instance Normalizable Signature where
	normalize (Signature n t)
		= Signature n $ normalize t




-- The (unique) identifier of a function. One identifier == one clause
type FunctionID	= (FQN, Int)

type FunctionBody	= [TClause]
data FunctionInfo	= FunctionInfo {declaredIn :: FQN, body :: FunctionBody}

{-The function table is a local function table, thus local for a single module-}
data FunctionTable
	= FunctionTable { locallyDeclared	:: Map Name Signature }

findExact	:: FunctionTable -> Signature -> Maybe FunctionInfo
findExact ft sign
		=  lookup sign ft
