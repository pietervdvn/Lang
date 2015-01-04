module Languate.TypeTable.BuildDocstringTable where

{--
This module implements the function which gets the docstring for each type.
--}

import StdDef
import Exceptions
import Languate.Checks.CheckUtils

import Languate.AST
import Languate.FQN
import Languate.World
import Languate.TypeTable

import Data.Map hiding (mapMaybe, map)
import Data.Maybe

buildDocstringTable	:: World -> Exc (Map TypeID String)
buildDocstringTable w	= do	let fqnMods	= toList $ modules w
				docStrings	<- mapM (uncurry buildDocstr) fqnMods
				return $ fromList $ concat docStrings

buildDocstr		:: FQN -> Module -> Exc [(TypeID, String)]
buildDocstr fqn m	=  do	let stms	= statements' m
				docstrs	<- mapM (declaredType' fqn) stms
				return $ catMaybes docstrs


declaredType'	:: FQN -> (Statement, Coor) -> Exc (Maybe (TypeID, String))
declaredType' fqn (stm, coor)
		= onLocation (fqn, coor) $
			declaredType fqn stm

declaredType	:: FQN -> Statement -> Exc (Maybe ((FQN, Name), String))
declaredType fqn  (ADTDefStm (ADTDef name _ _ docstring _))
		= unpack fqn name docstring
declaredType fqn (SynDefStm (SynDef name _ _ _ docstring))
		= unpack fqn name docstring
declaredType fqn (SubDefStm (SubDef name _ _ _ _ docstring))
		= unpack fqn name docstring
declaredType fqn (ClassDefStm classDef)
		= unpack fqn (name classDef) (classdocstr classDef)
declaredType _ _	= return $ Nothing


unpack		:: FQN -> Name -> Maybe String -> Exc (Maybe (TypeID, String))
unpack fqn name (Just doc)
		= return $ Just ((fqn,name), doc)
unpack _ name Nothing
		= do	warn $ "No docstring in the type declaration of "++name
			return Nothing
