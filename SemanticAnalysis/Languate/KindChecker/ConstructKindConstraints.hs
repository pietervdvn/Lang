module Languate.KindChecker.ConstructKindConstraints where
{--
This module implements the functions which calculate what kind a declaration has.

This means that some kinds can only be known the moment the entire kind table is visible.

--}
import StdDef
import Exceptions

import Languate.KindChecker.KindConstraint
import Languate.TAST
import Languate.AST
import Languate.FQN
import Languate.TypeTable
import Languate.World

import Data.Map (mapWithKey, findWithDefault, Map)
import Data.Maybe
import Control.Monad.Reader
import Control.Arrow




buildKindConstraintTable	:: Map FQN TypeLookupTable -> World -> Map FQN [(KindConstraint, (Coor, FQN))]
buildKindConstraintTable tlts w
	= 	let lookup' fqn	= findWithDefault (error $ "Bug: Type lookup table not found: (constructKindConstraints)"++show fqn) fqn	in
		mapWithKey (\fqn -> kindConstraints (lookup' fqn tlts) fqn) $ modules w

kindConstraints	::  TypeLookupTable -> FQN -> Module -> [(KindConstraint, (Coor, FQN))]
kindConstraints tlt fqn modul
		= concat $ runReader (mapM kindConstraintIn' $ statements' modul) (Info fqn tlt)


data Info	= Info {fqn :: FQN, tlt :: TypeLookupTable}
type RI a	= Reader Info a


kindConstraintIn'	:: (Statement, Coor) -> RI [(KindConstraint, (Coor, FQN))]
kindConstraintIn' (stm, coor)
			= do	constrs	<- kindConstraintIn stm
				(Info fqn _)	<- ask
				return $ zip constrs $ repeat (coor, fqn)

-- Kind of declares what relations of kinds between types exists. E.g. "Functor" has kind "a ~> b", "Maybe" has the same kind as "Functor" etc...
kindConstraintIn	:: Statement -> RI [KindConstraint]
kindConstraintIn (ADTDefStm (ADTDef name frees reqs _ _))
		= baseTypeConstr name frees reqs
kindConstraintIn (ClassDefStm classDef)
		= do	baseConstrs	<- baseTypeConstr (name classDef) (frees classDef) (classReqs classDef)
			base		<- resolve' (name classDef)
			constraints	<- subtypeConstraints base (frees classDef) (subclassFrom classDef)
			return $ baseConstrs ++ constraints
kindConstraintIn (InstanceStm (Instance id subtype reqs))
		= do	superT	<- resolve id
			subT	<- resolve subtype
			returnOne $ HaveSameKind subT superT
kindConstraintIn (SubDefStm (SubDef name _ frees superTypes reqs))
		= do	baseConstrs	<- baseTypeConstr name frees reqs
			subT	<- resolve' name
			constraints <- subtypeConstraints subT frees superTypes
			return $ baseConstrs ++ constraints
kindConstraintIn (SynDefStm (SynDef nm frees sameAs reqs))
		= do	synonym		<- resolve sameAs
			baseType	<- resolve' nm
			baseConstrs	<- baseTypeConstr nm frees reqs
			let base	= RApplied baseType $ map RFree frees
			let same	= HaveSameKind base synonym
			return $ same:baseConstrs
kindConstraintIn _	= return []


subtypeConstraints	:: RType -> [Name] -> [Type] -> RI [KindConstraint]
subtypeConstraints base frees superClasses
		= do 	superClasses	<- mapM resolve superClasses
			let appliedBase	= RApplied base $ map RFree frees
			return $ zipWith HaveSameKind (repeat appliedBase) superClasses

-- Constructs a basic 'has kind' relation, for the given (declared) name with it frees
baseTypeConstr	:: Name -> [Name] -> [TypeRequirement] -> RI [KindConstraint]
baseTypeConstr name frees reqs
		= do	base	<- _resolve' name
			(curry, constr)	<- buildCurry frees reqs
			return $ HasKind base curry : constr

-- builds the kind, based on frees. e.g. ["k","v"] becomes '' * ~> * ~> * ''. This might cause addition constraints, e.g. k is ''Eq'' and ''Ord''. This means ''Eq'' and ''Ord'' should have the same kind too
buildCurry	:: [Name] -> [TypeRequirement] -> RI (UnresolvedKind, [KindConstraint])
buildCurry frees reqs
		= do	reqs'	<- resolveReqs reqs
			buildCurry' frees reqs'

buildCurry'	:: [Name] -> [(Name, RType)] -> RI (UnresolvedKind, [KindConstraint])
buildCurry' [] reqs
		=  return (UKind, [])
buildCurry' (n:nms) reqs
	= do	(tail, constrs)	<- buildCurry' nms reqs
		let reqs'	= merge reqs
		let found	= fromMaybe [] $ lookup n reqs'
		return $ if null found then (UKindCurry UKind tail, constrs)
				else (UKindCurry (SameAs $ head found) tail, constrs ++ zipWith HaveSameKind found (tail' found) )



-- util methods
_resolve'	:: Name -> RI (FQN, Name)
_resolve' name	=  do	lt 	<- asks tlt
			return (_resolveType' lt ([], name), name)


resolve' name	= do	(fqn, nm)	<- _resolve' name
			return $ RNormal fqn nm

resolve		:: Type -> RI RType
resolve t	=  do	lt	<- asks tlt
			return $ resolveType lt t

resolveReqs	:: [TypeRequirement] -> RI [(Name, RType)]
resolveReqs rqs	=  do	lt	<- asks tlt
			return $ map (second (resolveType lt)) rqs


returnOne	:: a -> RI [a]
returnOne a	=  return [a]
