module Languate.TypeChecker.KindConstructor where
{--
This module implements the functions which calculate what kind a declaration has.

This means that some kinds can only be known the moment the entire kind table is visible.

--}

import Languate.KindChecker.KindConstraint
import Languate.TAST

-- Kind of declares what relations of kinds between types exists. E.g. "Functor" has kind "a ~> b", "Maybe" has the same kind as "Functor" etc...
kindConstraintIn	:: FQN -> Statement -> Maybe KindConstraint
kindConstraintIn fqn (ADTDefStm (ADTDef nm frees reqs _ _))
			= Just $ HasKind (asRType fqn ) $ _buildCurry frees reqs
kindConstraintIn fqn (SynDefStm (SynDef name frees typ))
			= Just $ HasSameKindAs
kindConstraintIn _	= Nothing


_buildCurry	:: [Name] -> Kind
_buildCurry []	=  Kind "*"
_buildCurry (nm:nms)
		=  KindCurry (Kind nm) $ _kcADTDef nms
