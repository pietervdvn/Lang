module Languate.KindChecker.KindConstraint where
{--
Implementation of kind constraint and related functions
--}

import Languate.TAST


-- reprsesents a kind where details should be still filled in.
data UnresolvedKind	= UKind	-- normal, simple kind
			| UKindCurry UnresolvedKind UnresolvedKind	-- normal, simple kindcurry
			| SameAs RType	-- means that the kind of rtype should be used
	deriving (Show, Eq)


-- actual constraints which should be checked in the end
data KindConstraint	= HaveSameKind RType RType	-- used for e.g. subtypes. ''' subtype Nat' = Nat & NatInf'  ''' makes sense, but ''' subtype Set = Collection a ''' does not ''' subtype Set a = Collection a ''' however does.
			| HasKind RType UnresolvedKind
	deriving (Show, Eq)
