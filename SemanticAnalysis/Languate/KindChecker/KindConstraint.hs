module Languate.KindChecker.KindConstraint where
{--
Implementation of kind constraint and related functions
--}

import Languate.TAST


-- represents a kindconstraint, e.g. Maybe has the same kind as Functor, RSA has the same kind as PubPrivAlg, applied to two args, ...
data KindConstraint	= HasKind RType Kind
			| HasSameKindAs RType RType
