module Languate.TypeTable.Bind.StMsg where

{--
This module implements utils for the state/either monad in which the binding happens
--}

import StdDef
import StateT

import Languate.TAST
import Languate.TypeTable hiding (spareSuperTypes, allSupertypes)

import Data.Map (Map, findWithDefault, lookup)
import qualified Data.Map as M

import Data.Set (Set)
import qualified Data.Set as S

import Data.Maybe
import Prelude hiding (fail, lookup, catch)

import Control.Monad.Trans


-- the monad we'll work with
type StMsg a	= StateT Context (Either String) a

data Context	= Context 	{ frees		:: Map Name [RType]	-- keeps track of the supertypes (= requirements) for a given free.
				, usedFrees	:: Set Name -- All knwon or used frees
               			, spareSuperTypes :: Map TypeID SpareSuperTypeTable
				, allSupertypes :: Map TypeID FullSuperTypeTable
				, binding_ 	:: Binding

				}
instance Show Context where
	show (Context frees _ _ _ b)= "Context "++sd frees ++ ", "++show b



-- ## Getters

requirementsOn	:: Name -> StMsg [RType]
requirementsOn a
	= get' frees |> findWithDefault [] a


getFstt	:: TypeID -> StMsg FullSuperTypeTable
getFstt tid
	= do	mFstt	<- get |> allSupertypes |> lookup tid
		assert (isJust mFstt) $ "No full super type table found for "++show tid
		return $ fromJust mFstt


getSstt	:: TypeID -> StMsg SpareSuperTypeTable
getSstt tid
 | tid == anyTypeID	= return M.empty
 | otherwise	= do	spareSTT	<- get |> spareSuperTypes |> lookup tid
			assert (isJust spareSTT) $ "No spare STT for "++show tid
			return $ fromJust spareSTT


addBinding	:: (Name, RType) -> StMsg ()
addBinding (n,t)
	= do	ctx	<- get
		let (Binding b)	= binding_ ctx
		-- check wether or not a conflicting binding exists
		let previous	= M.lookup n b
		assert (isNothing previous || t == fromJust previous) $
			"Conflicting bindings for '"++n++"' are found."++
			" It could be both bound to "++show (fromJust previous)++" and "++show t
		put $ ctx {binding_ = Binding $ M.insert n t b}

addFrees	:: [Name] -> StMsg ()
addFrees bound
	= do	ctx	<- get
		let frees'	= S.union (usedFrees ctx) (S.fromList bound)
		put $ ctx {usedFrees = frees'}

getUsedFrees	:: StMsg (Set Name)
getUsedFrees	= get' usedFrees


getBinding	:: StMsg Binding
getBinding	= get' binding_


-- ## Monad tools

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

catch'	:: StMsg a -> StMsg (Either String a)
catch' stmsg
	= do	ctx	<- get
		case runstateT stmsg ctx of
			(Left msg)	-> return $ Left msg
			(Right (a,ctx))	-> put ctx >> return (Right a)


try		:: StMsg a -> StMsg a -> StMsg a
try first backup
	= do	ctx	<- get
 		case runstateT first ctx of
			Left msg	-> backup
			Right (a, ctx')	-> put ctx' >> return a

inside		:: String -> StMsg a -> StMsg a
inside msg m	=  do	ctx	<- get
			case runstateT m ctx of
				Left msg'	-> fail $ msg++":\n"++msg'
				Right (a,ctx')	-> put ctx' >> return a

-- Tries the given action. If it fails, rolls back the binding and returns false
succeeded	:: StMsg a -> StMsg Bool
succeeded m	= catch False (m >> return True)

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
