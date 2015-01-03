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
				return $ fromList $ concatMap (uncurry buildDocstr) fqnMods


buildDocstr		:: FQN -> Module -> [(TypeID, String)]
buildDocstr fqn m	=  let stms	= statements m in
				mapMaybe (declaredType fqn) stms


declaredType	:: FQN -> Statement -> Maybe ((FQN, Name), String)
declaredType fqn  (ADTDefStm (ADTDef name _ _ docstring _))
		= Just ((fqn,name),docstring)
{-
declaredType fqn (SynDefStm (SynDef name _ _ _))
		= Just name
declaredType fqn (SubDefStm (SubDef name _ _ _ _ ))
		= Just name
declaredType fqn (ClassDefStm classDef)
		= Just $ name classDef -}
declaredType _ _	= Nothing
