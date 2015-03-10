module Languate.TypeTable.Bind.Unificate where

{--

This module implements simple unification.

--}

import StdDef
import StateT

import Languate.TAST
import Languate.TypeTable
import Languate.TypeTable.Bind.StMsg

import Data.Map (Map, findWithDefault, lookup)
import qualified Data.Map as M

import Data.Set (Set)
import qualified Data.Set as S

import Control.Monad

{- Tries to make two types the same, by filling in the frees in any of them.

Unificate is associative.

Type requirements and supertypes are **not** taken in account here.

Used in "add binding", if conflicting values could be added

-}
unificate	:: RType -> RType -> Either String Binding
unificate t0 t1
 | t0 == t1	= return noBinding
 | otherwise	= let 	used	= [t0,t1] |> freesInRT & concat & S.fromList
			ctx	= Context M.empty used (error "No tt needed for unify!") noBinding
			res	= runstateT (unificate' t0 t1) ctx in
			res |> snd |> binding

unificate'	:: RType -> RType -> StMsg ()
unificate' (RFree a) (RFree b)
	= unless (a == b) $ do
		addBinding (a, RFree b)
		addBinding (b, RFree a)
unificate' (RFree a) t1
	= addBinding (a,t1)
unificate' t0 (RFree b)
	= addBinding (b,t0)
unificate' (RCurry t0 t1) (RCurry t0' t1')
	= do	unificate' t0 t0'
		unificate' t1 t1'
unificate' (RApplied bt at) (RApplied bt' at')
	= do	unificate' bt bt'
		unificate' at at'
unificate' t0 t1
	= assert (t0 == t1) $ "Could not unify "++ st True t0 ++" and "++ st True t1
