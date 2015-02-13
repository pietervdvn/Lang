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


{-

Binds t0 in t1. If binding succeeds, this means t0 is a subtype (or equal type) of t1.

For each applied type, at most one supertypetable convert can happen.
-}
bind	:: TypeTable -> RType -> RType -> Either String Binding
bind	= todo








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
