module Languate.KindChecker.ConstructKindConstraints where
{--
This module implements the functions which calculate what kind a declaration has.

This means that some kinds can only be known the moment the entire kind table is visible.

--}
import StdDef

import Languate.KindChecker.KindConstraint
import Languate.TAST
import Languate.AST
import Languate.FQN
import Languate.TypeTable

import Control.Monad.Reader
import Control.Arrow

import Data.Maybe

import Debug.Trace

data Info	= Info {fqn :: FQN, tlt :: TypeLookupTable}
type RI a	= Reader Info a

-- Kind of declares what relations of kinds between types exists. E.g. "Functor" has kind "a ~> b", "Maybe" has the same kind as "Functor" etc...
kindConstraintIn	:: Statement -> RI [KindConstraint]
kindConstraintIn (ADTDefStm (ADTDef name frees reqs _ _))
		= do	base	<- resolve' name
			kind	<- buildCurry frees reqs
			returnOne $ HasKind base kind
kindConstraintIn (ClassDefStm classDef)
		= do	base	<- resolve' $ name classDef
			kind	<- buildCurry (frees classDef) (classReqs classDef)
			subClasses	<- mapM resolve $ subclassFrom classDef
			let appliedBase	= RApplied base $ map RFree (frees classDef)
			let constraints	= zipWith HaveSameKind (repeat appliedBase) subClasses
			return $ HasKind base kind: constraints
kindConstraintIn (InstanceStm (Instance nm t))
		= do	instanc	<- resolve t
			instanceOf	<- resolve' nm
			returnOne $ HaveSameKind instanceOf instanc
kindConstraintIn _	= return []


buildCurry	:: [Name] -> [TypeRequirement] -> RI UnresolvedKind
buildCurry frees reqs
		= do	reqs'	<- resolveReqs reqs
			buildCurry' frees reqs'

buildCurry'	:: [Name] -> [(Name, RType)] -> RI UnresolvedKind
buildCurry' [] reqs
		=  return UKind
buildCurry' (n:nms) reqs
	= do	tail	<- buildCurry' nms reqs
		return $ flip UKindCurry tail $
			fromMaybe UKind $ fmap SameAs $ lookup n reqs

resolve'	:: Name -> RI RType
resolve' name	=  do	lt 	<- asks tlt
			return $ resolveType' lt ([], name)

resolve		:: Type -> RI RType
resolve t	=  do	lt	<- asks tlt
			return $ resolveType lt t

resolveReqs	:: [TypeRequirement] -> RI [(Name, RType)]
resolveReqs rqs	=  do	lt	<- asks tlt
			return $ map (second (resolveType lt)) rqs


returnOne	:: a -> RI [a]
returnOne a	=  return [a]
