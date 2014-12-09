module Languate.TypeTable.BuildTypeLookupTable where

{-
This module provides functions to
	-> Calculate what types are declared in a module
	-> What types are public in a module
	-> builds the type lookup table for a given set of modules
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


buildTypeLookupTable	:: World -> Map FQN TypeLookupTable
buildTypeLookupTable world
	= let 	locally	= mapWithKey (\fqn mod -> injectSet fqn $ locallyDeclared mod) $ modules world
		err n	= error $ "Building type lookup table: fqn not found: " ++ show n
		localFor n	= findWithDefault (err n) n locally
		publicly	= fmap isPublicType $ modules world
		isPublic t n	= (findWithDefault (err n) n publicly) t
		exports	= calculateExports' world localFor isPublic
		known	= calculateImports' world localFor exports
		kys	= keys $ modules world in
		M.fromList $ zip kys $ map (buildTLT (todos "Aliasses for imports" <error<><)isPublic known) $ kys

buildTLT	:: Map FQN Name -> ((FQN,Name) -> FQN -> Bool) -> Map FQN (Set (FQN, Name)) -> FQN -> Map ([Name], Name) [(FQN, Visible)]
buildTLT aliasses isPublic known fqn
		= let	isPublic' n	= if isPublic (fqn,n) fqn then Public else Private
			known' 		= findWithDefault (S.empty) fqn known
			names2TypeDict		= (map (\(fqn', simpleName) -> (namesFor aliasses fqn' simpleName,(fqn', isPublic' simpleName)) ) $ S.toList known') :: [( [([Name],Name)] , (FQN, Visible))]
			tlt	= merge $ map swap $ unmerge $ map swap names2TypeDict in
			M.fromList tlt



-- first arg: table of aliasses. E.g. Data.Bool as B -> {Data.Bool -> B}. Not in DB: no alias (or not imported)
namesFor	:: Map FQN Name -> FQN -> Name -> [([Name], Name)]
namesFor aliasses fqn nm
		= [ (prefixPath,nm) | prefixPath <- tails $ fromMaybe (modulePath fqn) $ fmap (:[]) $ M.lookup fqn aliasses ]



injectSet	:: (Ord a, Ord b) => a -> Set b -> Set (a,b)
injectSet a	=  S.map (\b -> (a,b))

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
-- a type is public if no public function uses this type
isPublicType	:: Module -> (FQN, Name) -> Bool
isPublicType mod (_, name)
		= let	publicFuncs	= publicFunctions (exports mod) (statements mod)
			publicTypes	= S.fromList $ concatMap usedTypes $ concatMap allTypes publicFuncs in
			name `S.member` publicTypes

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

-- types which are locally declared (for usage analysis.) Module.TYpe is thus never really needed
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
