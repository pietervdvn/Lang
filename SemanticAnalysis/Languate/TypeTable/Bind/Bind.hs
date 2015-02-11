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
import Data.Either
import Prelude hiding (fail, lookup)

import StateT

import Languate.TAST
import Languate.TypeTable

import Control.Monad hiding (fail)
import Control.Monad.Trans

import Debug.Trace

{- Tries to make two types the same, by filling in the frees in any of them.

Unificate is associative.

Type requirements and supertypes are **not** taken in account here.

-}
unificate	:: RType -> RType -> Either String Binding
unificate t0 t1
	| t0 == t1	= return noBinding
	| otherwise	= let 	ctx	= Context M.empty (error "No tt needed for unify!") noBinding
				res	= runstateT (unificate' t0 t1) ctx in
				res |> snd |> binding

unificate'	:: RType -> RType -> StMsg ()
unificate' (RFree a) (RFree b)
	= do	addBinding (a, RFree b)
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















data Context	= Context 	{ frees		:: Map Name [RType]	-- keeps track of the supertypes (= requirements) for a given free. All bound frees should be included.
				, typeT 	:: TypeTable
				, binding 	:: Binding
				}

-- the monad we'll work with
type StMsg a	= StateT Context (Either String) a




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


sstt	:: TypeID -> StMsg SpareSuperTypeTable
sstt tid
	= do	spareSTT	<- get' typeT |> spareSuperTypes |> lookup tid
		assert (isJust spareSTT) $ "No spare STT for "++show tid
		return $ fromJust spareSTT


addBinding	:: (Name, RType) -> StMsg ()
addBinding (n,t)
	= do	ctx	<- get
		let (Binding b)	= binding ctx
		-- check wether or not a conflicting binding exists
		let previous	= M.lookup n b
		when (isJust previous) $ unificate' (RFree n) (fromJust previous)
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
	= do	ctx	<- get
 		case runstateT first ctx of
			Left msg	-> backup
			Right (a, ctx')	-> put ctx' >> return a

(?)	:: Maybe a -> String -> StMsg a
(?) Nothing
	= fail
(?) (Just a)
	= const $ return a

joinEither	:: [Either String b] -> Either String [b]
joinEither []	= Right []
joinEither (Left str:rest)
	= case joinEither rest of
		Left msg	-> Left $ str++"; "++msg
		Right _		-> Left str
joinEither (Right b:rest)
	= case joinEither rest of
		Left msg	-> Left msg
		Right bs	-> Right (b:bs)


-- Fails if requirements on the next type are returned. Assumes m return [], fails otherwise
noReqs	:: StMsg [a] -> StMsg ()
noReqs m
	= do	reqs	<- m
		assert (L.null reqs) "No requirements should be returned."


instance Show Context where
	show (Context frees _ b)= "Context "++sd frees ++ ", "++show b
