module Languate.KindChecker.KindConstraint where
{--
Implementation of kind constraint and related functions
--}


import StdDef
import Languate.TAST
import Languate.FQN


-- reprsesents a kind where details should be still filled in.
data UnresolvedKind	= UKind	-- normal, simple kind
			| UKindCurry UnresolvedKind UnresolvedKind	-- normal, simple kindcurry
			| SameAs RType	-- means that the kind of rtype should be used
	deriving (Show, Eq)


-- actual constraints which should be checked in the end
data KindConstraint	= HaveSameKind RType RType	-- used for e.g. subtypes. ''' subtype Nat' = Nat & NatInf'  ''' makes sense, but ''' subtype Set = Collection a ''' does not ''' subtype Set a = Collection a ''' however does. These are requirements that should be checked independantly
			| HasKind (FQN, Name) UnresolvedKind
	deriving (Show, Eq)

isHasKind (HasKind _ _)	= True
isHasKind _		= False


numberOfArgs	:: UnresolvedKind -> Maybe Int
numberOfArgs UKind	= Just 0
numberOfArgs (UKindCurry _ tail)
			= numberOfArgs tail |> (+1)
numberOfArgs _		= Nothing


numberOfArgs'	:: KindConstraint -> Maybe Int
numberOfArgs' (HasKind _ uk)
		= numberOfArgs uk
numberOfArgs' _	= Nothing

-- all the dependencies!
dependsOn	:: UnresolvedKind -> [(FQN, Name)]
dependsOn (SameAs rt)
		= dependsOn' rt
dependsOn _	= []

dependsOn'	:: RType -> [(FQN, Name)]
dependsOn' (RNormal fqn nm)
		= [(fqn, nm)]
dependsOn' (RApplied r rs)
		= concatMap dependsOn' (r:rs)
dependsOn' (RCurry rs)
		= concatMap dependsOn' rs
dependsOn' (RTuple rs)
		= concatMap dependsOn' rs
