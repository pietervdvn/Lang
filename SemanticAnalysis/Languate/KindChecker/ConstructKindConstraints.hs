module Languate.KindChecker.ConstructKindConstraints where
{--
This module implements the functions which calculate what kind a declaration has.

This means that some kinds can only be known the moment the entire kind table is visible.

--}
import StdDef
import Exceptions

import Languate.KindChecker.KindConstraint
import Languate.TAST
import Languate.AST as AST
import Languate.FQN
import Languate.TypeTable
import Languate.World
import Languate.Checks.CheckUtils

import Data.Map (mapWithKey, findWithDefault, Map)
import qualified Data.Map as M
import Data.Maybe
import Control.Monad.Reader
import Control.Arrow




buildKindConstraints	:: Map FQN TypeLookupTable -> World -> Exc [(KindConstraint, Location)]
buildKindConstraints tlts w
	= do	let mods	= M.toList $ modules w
		mapM (uncurry $ kindConstraints tlts) mods |> concat

kindConstraints	:: Map FQN TypeLookupTable -> FQN -> Module -> Exc [(KindConstraint, Location)]
kindConstraints tlts fqn modul
		= inFile fqn $  do
			tlt	<- M.lookup fqn tlts ? ("Bug: Type lookup table not found: (constructKindConstraints)"++show fqn)
			constraints <- mapM (kindConstraintIn' (Info fqn tlt)) $ statements' modul
			return $ concat constraints


data Info	= Info {fqn :: FQN, tlt :: TypeLookupTable}
type RI a	= ReaderT Info (Exceptions String String) a




kindConstraintIn'	:: Info -> (Statement, Coor) -> Exc [(KindConstraint, Location)]
kindConstraintIn' inf@(Info fqn _) (stm, coor)
			= onLine coor $ do
				constrs	<- runReaderT (kindConstraintIn stm) inf
				return $ zip constrs $ repeat (fqn, coor)

-- Kind of declares what relations of kinds between types exists. E.g. "Functor" has kind "a ~> b", "Maybe" has the same kind as "Functor" etc...
kindConstraintIn	:: Statement -> RI [KindConstraint]
kindConstraintIn (ADTDefStm (ADTDef name frees reqs _ _))
		= do	id		<- getId name
			baseTypeConstr id frees reqs
kindConstraintIn (ClassDefStm classDef)
		= do	id@(fqn, nm)	<- getId $ name classDef
			let frees	=  AST.frees classDef
			let reqs	=  classReqs classDef
			baseConstrs	<- baseTypeConstr id frees reqs
			constraints	<- subtypeConstraints (RNormal fqn nm) frees $ subclassFrom classDef
			return $ baseConstrs ++ constraints
kindConstraintIn (InstanceStm (Instance nm subtype reqs))
		= do	superT	<- resolve nm
			subT	<- resolve subtype
			returnOne $ HaveSameKind subT superT
kindConstraintIn (SubDefStm (SubDef name _ frees superTypes reqs _))
		= do	id@(fqn,_)	<- getId name
			baseConstrs	<- baseTypeConstr id frees reqs
			constraints 	<- subtypeConstraints (RNormal fqn name) frees superTypes
			return $ baseConstrs ++ constraints
kindConstraintIn (SynDefStm (SynDef nm frees sameAs reqs _))
		= do	synonym		<- resolve sameAs
			id@(fqn, _)	<- getId nm
			baseConstrs	<- baseTypeConstr id frees reqs
			let base	= RNormal fqn nm
			let same	= HaveSameKind base synonym
			return $ same:baseConstrs
kindConstraintIn _	= return []


subtypeConstraints	:: RType -> [Name] -> [Type] -> RI [KindConstraint]
subtypeConstraints base frees superClasses
		= do 	superClasses	<- mapM resolve superClasses
			let appliedBase	= RApplied base $ map RFree frees
			return $ zipWith HaveSameKind (repeat appliedBase) superClasses

-- Constructs a basic 'has kind' relation, for the given (declared) name with it frees
baseTypeConstr	:: TypeID -> [Name] -> [TypeRequirement] -> RI [KindConstraint]
baseTypeConstr id frees reqs
		= do	(curry, constr)	<- buildCurry frees reqs
			return $ HasKind id curry : constr

-- builds the kind, based on frees. e.g. ["k","v"] becomes '' * ~> * ~> * ''. This might cause addition constraints, e.g. k is ''Eq'' and ''Ord''. This means ''Eq'' and ''Ord'' should have the same kind too
buildCurry	:: [Name] -> [TypeRequirement] -> RI (UnresolvedKind, [KindConstraint])
buildCurry frees reqs
		= do	reqs'	<- resolveReqs reqs |> merge
			buildCurry' frees reqs'

buildCurry'	:: [Name] -> [(Name, [RType])] -> RI (UnresolvedKind, [KindConstraint])
buildCurry' [] reqs
		=  return (UKind, [])
buildCurry' (n:nms) reqs
	= do	(tail, constrs)	<- buildCurry' nms reqs
		let found	= fromMaybe [] $ lookup n reqs
		return $ if null found then (UKindCurry UKind tail, constrs)
				else (UKindCurry (SameAs $ head found) tail, constrs ++ zipWith HaveSameKind found (tail' found) )



-- UTILS

-- resolves a single type
resolve		:: Type -> RI RType
resolve t	=  do	lt	<- asks tlt
			lift $ resolveType lt t

getId		:: Name -> RI TypeID
getId nm	=  do	fqn'	<- asks fqn
			return (fqn', nm)

resolveReqs	= mapM resolveReq

resolveReq	:: TypeRequirement -> RI (Name, RType)
resolveReq (nm, t)	=  do	rt	<- resolve t
				return (nm, rt)


returnOne	:: a -> RI [a]
returnOne a	=  return [a]
