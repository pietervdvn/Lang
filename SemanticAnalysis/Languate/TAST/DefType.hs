module Languate.TAST.DefType where

{--
This module implements only the definitions of TAST and built-in fixed types and values
--}

import StdDef

import Languate.FQN
import Languate.AST


type TypeID	= (FQN, Name)



{- Each type has a kind. You can think of those as the 'type of the type'
E.g. a 'Maybe' is always in function of a second argument, e.g. 'Maybe Int' or 'Maybe String'.
'String' and ''Functor Int'' have a kind ''a'' (no arguments), whereas ''Functor'' has a kind ''a -> b''
Type requirements are **not** stored into the kind, these are checked seperatly.

e.g.
type True a b	= a	:: * ~> * ~> *
type False a b	= b	:: * ~> * ~> *
type And b1 b2 a b = b2 (b1 a b) b
			:: (* ~> * ~> *) ~> (* ~> * ~> *) ~> *

-}
data Kind		= Kind
			| KindCurry Kind Kind -- Kind curry: Dict a b :: a ~> (b ~> *)
	deriving (Ord, Eq)



-- resolved type -- each type known where it is defined
data RType	= RNormal FQN Name
			| RFree String
			| RApplied RType RType
			| RCurry RType RType
	deriving (Eq, Ord)


type RTypeReq		= [(Name, [RType])]
-- a type with constraints
type CType		= (RType, RTypeReq)
{- A type of a function with multiple 'tags', e.g. (+) : Nat -> Nat -> Nat & Comm Nat Nat
	([Nat -> Nat -> Nat, Comm Nat Nat], [{-no reqs-}])
	-}
type CTypeUnion		= ([RType], RTypeReq)



{- a signature for a single implementation.
All types (union + reqs) live withing the same context, thus a free type var is the same everywhere
-}
data Signature		= Signature
				{ signFQN	:: FQN	-- where the function is defined
				, signName	:: Name	-- its name
				, signTypes	:: CTypeUnion	-- all the types it obeys
				}
	deriving (Eq, Ord)

asSignature	:: (FQN, Name, CTypeUnion) -> Signature
asSignature (fqn, n, union)
	= Signature fqn n union
