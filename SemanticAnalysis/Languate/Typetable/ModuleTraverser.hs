module Languate.Typetable.ModuleTraverser (locallyDeclared, declaredType) where

{-
Multiple functions to traverse the statements, in search for type declaration stuff
-}

import StdDef


import Languate.Typetable.TypeLookupTable.TypeLookupTableDef

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Set hiding (map, filter)
import Data.Map hiding (map, filter, mapMaybe)
import Data.Maybe

import Languate.AST
import Languate.TAST
import Languate.FQN
import Languate.Package


-- All locally declared types
locallyDeclared	:: Module -> [(Name, [Name], [TypeRequirement])]
locallyDeclared mod
		=  statements mod |> declaredType & catMaybes



declaredType :: Statement -> Maybe (Name, [Name], [TypeRequirement])
declaredType (ADTDefStm (ADTDef name frees reqs _ _))
		= Just (name, frees, reqs)
declaredType (SubDefStm (SubDef name _ frees _ reqs))
		= Just (name, frees, reqs)
declaredType (ClassDefStm classDef)
		= Just (name classDef, frees classDef, classReqs classDef)
declaredType _	= Nothing



declaredSuperType	:: Statement -> Maybe ((Name, [Name]), Name, [TypeRequirement])
declaredSuperType (ADTDefStm (ADTDef nm frees reqs _ adopts))
	= todo
declaredSuperType (InstanceStm (Instance typePath frees super reqs))
	= Just $
declaredSuperType _	= Nothing
