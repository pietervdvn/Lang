module Languate.TypeTable.BuildTypeTable where

{--
This module builds the type table for the given module.

-> we build an import table (which states what modules are imported with which aliases) (ImportTable/ImportTable.hs)
-> pass 1 : we build a simple set with what locally declared types (correctness doesn't matter)
	-> we check what types are public (according to the functions)
-> we calculate the export and imports for each module (exportCalculator)

We now have a clear sight which module can view what types, and where this was originally implemented.
	-> We can resolve type calls of give an error when ambiguity arises

-> Then, we build the class, synonym, instance and subtype defs per module
	-> Error on cycles in the class defs

-> We can now cherrypick the implementation details to build the type table


------

https://www.haskell.org/haskellwiki/GADTs_for_dummies

type synonyms act as functions, with kinds we can do arity checks
data defs and class defs as new declarations (no kind magic here, just simple declarations)
instance defs for predicates
subtype defs for predicates

Type Requirements are passed when needed and implicit

--}

import qualified Data.Set as S
import Data.Set hiding (map)
import Data.Maybe
import Languate.AST
import StdDef

-- pass 1
locallyDeclared	:: Module -> Set Name
locallyDeclared	mod
		=  S.fromList $ catMaybes $ map declaredType $ statements mod

declaredType	:: Statement -> Maybe Name
declaredType (ADTDefStm (ADTDef name _ _ _ _))
		= Just name
declaredType (SynDefStm (SynDef name _ _ _))
		= Just name
declaredType (SubDefStm (SubDef name _ _ _ ))
		= Just name
declaredType (ClassDefStm classDef)
		= Just $ name classDef
declaredType _	= Nothing

-- pass 1.5
-- a type is public if no public function uses this type
isPublicType	:: Module -> Name -> Bool
isPublicType mod name
		= let	publicFuncs	= publicFunctions (exports mod) (statements mod)
			publicTypes	= map (usedTypes . snd) publicFuncs in
			name `elem` publicTypes

publicFunctions	:: Restrict -> [Statement] -> [(Name, Type)]
publicFunctions restrict stms
		= _censor restrict $ map _unpackF stms

_censor		:: Restrict -> [(Name, Type)] -> [(Name, Type)]
_censor restrict
		= filter (isAllowed restrict . fst)

_unpackF	:: Statement -> [(Name,Type)]
_unpackF (FunctionStm f)
		= signs f
_unpackF	= []


usedTypes	:: Type -> [Name]
usedTypes (Normal n)
		= [n]
usedTypes (Applied t ts)
		= usedTypes t ++ concatMap usedTypes ts
usedTypes (Curry ts)
		= concatMap usedTypes ts
usedTypes (TupleType ts)
		= concatMap usedTypes ts
usedTypes _	= []
