module Languate.TypeTable where

{-

Everythin we know about every type we know of.

The type table keeps track of relations between types.

The supertype keeps track of what type 'implements' what interface
Supertypes can be achieved by implementing an interface, subtype HTML = String,
This means that a type can have multiple, unrelated, direct subtypes.
e.g.
Bool is [Eq, Ord, Monoid, Show, ...]
-}

import StdDef
import Data.Map as M
import Data.Set as S
import Data.List (intercalate)
import Languate.AST
import Languate.TAST
import Languate.FQN

{-
The type table contains all known types within a certain module.

-}

data Duplicate	= Duplicate [FQN]
data TypeTable	= TypeTable	{ kinds		:: Map RType Kind
				, typeReqs	:: Map (RType, Int) (Set RType)	-- type requirements are implicit; contains synonyms
				, supertypes	:: Map RType (Set RType)	-- direct super types. should have the same kind. E.g. String in List Char; both are *
				, synonyms	:: Map RType RType	-- should have the same kind. Acts as an 'equivalence/equality' relation
				, revSynonyms	:: Map RType (Set RType)	-- inverse relation of synonyms
				{-
				Tells what functions should be implemented to be an instance of given superclass
				In TypeTable and not in instanceConstr: not a class def!
				-}
				, instConstr	:: Map RType (ClassDef, Kind)}
	deriving (Show, Ord, Eq)

-- basically the same as the aliastable, but with types.
type TypeLookupTable	= Map ([Name], Name) (Set FQN)	-- mutliple values, multiple possiblities in some cases!

resolveType	:: TypeLookupTable -> Type -> RType
resolveType tlt (Normal path name)
		= RNormal (_resolveType' tlt (path, name)) name
resolveType tlt (Free nm)
		= RFree nm
resolveType tlt (Applied t tps)
		= RApplied (resolveType tlt t) $ fmap (resolveType tlt) tps
resolveType tlt (Curry tps)
		= RCurry $ fmap (resolveType tlt) tps
resolveType tlt (TupleType tps)
		= RTuple $ fmap (resolveType tlt) tps



resolveType'	:: TypeLookupTable -> ([Name], Name) -> RType
resolveType' tlt (path, name)
		= RNormal (_resolveType' tlt (path, name)) name


_resolveType'	:: TypeLookupTable -> ([Name], Name) -> FQN
_resolveType' tlt k@(mods,t)
	= let 	repr		= intercalate "." $ mods ++ [t]
		notFoundErr	= error $ "Type error: the following type is not declared or imported: " ++ repr
		tps	= findWithDefault notFoundErr k tlt
		ambigErr	= error $ "Type error: the type "++ repr ++ " can both resolve to: "++ show tps in
		if S.null tps	then notFoundErr
			else if S.size tps == 1	then S.findMin tps else ambigErr


showTLT dict	=  intercalate "; " $  fmap sitem $ M.toList dict

sitem (k, possib)	= spth k ++ " --> {"++intercalate ", " (fmap shwFQN possib)  ++ "}"
shwFQN (fqn, _)	= intercalate "." $ modulePath fqn
spth (nms, nm)	= intercalate "." $ nms ++ [nm]
