module Languate.TypeTable.BuildTypeLookupTable where

{-
This module provides functions to
	-> Calculate what types are declared in a module
	-> What types are public in a module
	-> Get the aliastable
	-> For Module M, expand the aliastable
 -}

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Set hiding (map, filter)
import Data.Map hiding (map, filter)
import Data.Maybe
import Data.Tuple
import Control.Arrow (first)
import Data.List
import Languate.AST
import StdDef

import Languate.World
import Languate.FQN
import Languate.TypeTable.TypeTable
import Languate.ImportTable.ExportCalculator

import Debug.Trace



--buildTLTs	:: World -> Map FQN TypeLookupTable
buildTLTs world	=  let	modules	= Languate.World.modules world
			injectSet a	= S.map (\b -> (a,b))				-- small helper function
			locally = mapWithKey (\fqn mod -> injectSet fqn $ locallyDeclared mod) modules	-- {FQN -> What types are declared here}
			err fqn = error $ "Building type lookup table: fqn not found: " ++ show fqn
			localFor fqn = findWithDefault (err fqn) fqn locally		-- function to ask: what types are declared in this fqn
			publicly = M.fromList $ fmap (\fqn -> (fqn, isPublicType' world fqn)) $ keys modules	:: Map FQN ((FQN, Name) -> Bool)
			isPublic t fqn = (findWithDefault (err fqn) fqn publicly) t	-- function to ask: what types are public/exported from this fqn
			exports = calculateExports' world localFor isPublic in		-- {FQN (1) -> {FQN (2) -> Name (3)}}: This fqn (1) exports these type(names) (3), which are originally declared on location (2). E.g. {"Prelude" --> {"Data.Bool" --> "Bool", "Num.Nat" --> "Nat", "Num.Nat" --> "Nat'", ...}, ...}
			exports	-- M.map (buildTLT exports) $ aliasTables world


{- builds a TLT for a certain module. The alias table is taken, and the fqn's expanded into the known types for the module.
args:
 - Local module info
 - Exports: {FQN --> {FQN --> Name}}
-}
buildTLT	:: Map FQN (Map FQN Name) -> AliasTable -> TypeLookupTable
buildTLT at exports
		= todo


-- pass 1
locallyDeclared	:: Module -> Set Name
locallyDeclared	mod
		=  S.fromList $ catMaybes $ map declaredType $ statements mod

declaredType	:: Statement -> Maybe Name
declaredType (ADTDefStm (ADTDef name _ _ _ _))
		= Just name
declaredType (SynDefStm (SynDef name _ _ _))
		= Just name
declaredType (SubDefStm (SubDef name _ _ _ _ ))
		= Just name
declaredType (ClassDefStm classDef)
		= Just $ name classDef
declaredType _	= Nothing

-- pass 1.5

isPublicType'	:: World -> FQN -> (FQN, Name) -> Bool
isPublicType' w fqn
		=  let	err 	= error $ "Building type lookup table: fqn not found: "++show fqn
			fwd	= findWithDefault err fqn
			modul	= fwd $ modules w
			imps	= fwd $ importGraph' w in
			isPublicType imps modul

-- a type is public if 1) at least one public function uses this type or 2) the import was public, thus the given FQN was imported publicly
isPublicType	:: Map FQN Import -> Module -> (FQN, Name) -> Bool
isPublicType imps mod (fqn, name)
		= let	publicFuncs	= publicFunctions (exports mod) (statements mod)
			publicTypes	= S.fromList $ concatMap usedTypes $ concatMap allTypes publicFuncs
			publicFQN	= (\(Import vis _ _ _ _) -> vis == Public) $ findWithDefault (Import Private todo todo todo todo) fqn imps in
			publicFQN || name `S.member` publicTypes



publicFunctions	:: Restrict -> [Statement] -> [(Name, Type, [TypeRequirement])]
publicFunctions restrict stms
		= _censor restrict $ concatMap _unpackF stms

_censor		:: Restrict -> [(Name, Type, [TypeRequirement])] -> [(Name, Type, [TypeRequirement])]
_censor restrict
		= filter (\(nm,_,_) -> isAllowed restrict nm)

_unpackF	:: Statement -> [(Name,Type, [TypeRequirement])]
_unpackF (FunctionStm f)
		= signs f
_unpackF (ClassDefStm cd)
		= fmap (\(nm,t,_,tr) -> (nm,t,tr)) $ decls cd
_unpackF _	= []

-- calculates which types are used in the type. This way we know what types are used and might be public
usedTypes	:: Type -> [Name]
usedTypes (Normal [] n)
		= [n]
usedTypes (Applied t ts)
		= usedTypes t ++ concatMap usedTypes ts
usedTypes (Curry ts)
		= concatMap usedTypes ts
usedTypes (TupleType ts)
		= concatMap usedTypes ts
usedTypes _	= []

allTypes	:: (Name, Type, [TypeRequirement]) -> [Type]
allTypes (_,t,treqs)
		= [t] ++ map snd treqs
