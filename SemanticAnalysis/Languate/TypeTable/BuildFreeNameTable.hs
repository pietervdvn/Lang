module Languate.TypeTable.BuildFreeNameTable where

{--
Table which keeps track what names are given to the free variables.
E.g. Dict k v :{(Collections.Dict,Dict) --> {0 --> "k",1 --> "v"}
--}

import StdDef
import Exceptions
import Languate.Checks.CheckUtils

import Data.Map as M
import Data.Maybe

import Languate.World
import Languate.FQN
import Languate.AST
import Languate.TypeTable


buildFreeNameTable	:: World -> Exc (Map TypeID (Map Int Name))
buildFreeNameTable w	= do	let fqnMods	= toList $ modules w
				table	<- mapM (uncurry buildTables) fqnMods
				return $ fromList $ concat table


buildTables		:: FQN -> Module -> Exc [(TypeID, (Map Int Name))]
buildTables fqn m	=  do	let stms	= statements' m
				docstrs	<- mapM (declaredType' fqn) stms
				return $ catMaybes docstrs


declaredType'	:: FQN -> (Statement, Coor) -> Exc (Maybe (TypeID, Map Int String))
declaredType' fqn (stm, coor)
		= onLocation (fqn, coor) $
			declaredType fqn stm

declaredType	:: FQN -> Statement -> Exc (Maybe (TypeID, Map Int String))
declaredType fqn  (ADTDefStm (ADTDef name frees _ _ _))
		= unpack fqn name frees
declaredType fqn (SynDefStm (SynDef name frees _ _ _))
		= unpack fqn name frees
declaredType fqn (SubDefStm (SubDef name _ frees _ _ _))
		= unpack fqn name frees
declaredType fqn (ClassDefStm classDef)
		= unpack fqn (name classDef) (frees classDef)
declaredType _ _	= return $ Nothing


unpack		:: FQN -> Name -> [String] -> Exc (Maybe (TypeID, Map Int String))
unpack fqn nm frees
		= do	let id	= (fqn, nm)
			let core	= fromList $ zip [0..] frees
			return $ Just (id, core)
