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
import Exceptions

import Data.Map as M
import Data.Set as S
import Data.List (intercalate)
import Languate.AST
import Languate.TAST
import Languate.FQN
import Languate.Checks.CheckUtils

import Control.Arrow
import Normalizable
{-
The type table contains all known types within a certain module.

-}

type TypeID	= (FQN, Name)

type TypeReqTable	= Map (TypeID, Int) (Set RType)
type KindLookupTable	= Map TypeID Kind

{-
Keeps track of supertype relations.

E.g.

instance List (a:Eq) is Eq
instance List (a:Eq) is Set a
instance Dict k (v:Dict k k) is Multigraph

This is saved as
{List --> { [] -> Collection
	    ["a"] --> Eq	, where "a:Eq"
	    ["a"] --> Show	, where "a:Show"
	    ["a"] --> Set a}}
-}


type SuperTypeTableFor	= Map [Name] (Set RType, Map Name [RType])
type SuperTypeTable	= Map TypeID SuperTypeTableFor

data TypeTable	= TypeTable	{ kinds		:: KindLookupTable
				, typeReqs	:: TypeReqTable			-- type requirements are explicit for new type declarations; contains synonyms
				, supertypes	:: SuperTypeTable	-- direct super types. should have the same kind. E.g. String in List Char; both are *
				, docstrings	:: Map TypeID String
				, freeNames	:: Map TypeID (Map Int Name)}
	deriving (Show, Ord, Eq)

-- basically the same as the aliastable, but with types.
type TypeLookupTable	= Map ([Name], Name) (Set FQN)	-- mutliple values, multiple possiblities in some cases!


-- Finds the type within the TLT
findTypeOrigin	:: TypeLookupTable -> ([Name], Name) -> Exc FQN
findTypeOrigin tlt id@(path, t)
	=  do	fqnSet	<- M.lookup id tlt ? ("The type "++spth id ++" could not be resolved. It is not declared or imported.")
		let fqns	= S.toList fqnSet
		assert (isSingleton fqns) $ "The type "++spth id++" is ambigous. We found it in modules "++show fqns
		return $ head fqns


findTypeOrigin'	:: TypeLookupTable -> ([Name],Name) -> Exc (FQN, Name)
findTypeOrigin' tlt id@(_, nm)
		= do	fqn	<- findTypeOrigin tlt id
			return (fqn, nm)




resolveType	:: TypeLookupTable -> Type -> Exc RType
resolveType tlt (Normal path name)
		= do	fqn	<- findTypeOrigin tlt (path, name)
			return $ RNormal fqn name
resolveType tlt (Free nm)
		= return $ RFree nm
resolveType tlt e@(Applied t tps)
		= _construct tlt e (t:tps) (\(rt:rtps) -> RApplied rt rtps)
resolveType tlt e@(Curry tps)
		= _construct tlt e tps RCurry
resolveType tlt e@(TupleType tps)
		= _construct tlt e tps RTuple
resolveType _ Infer
		= halt "Unresolved infer"

resolveTypes tlt
		= mapM (resolveType tlt)

_construct	:: TypeLookupTable -> Type -> [Type] -> ([RType] -> RType) -> Exc RType
_construct tlt e tps cons
		=  inside ("In the type expression "++show e) $ do
			rtps	<- mapM (resolveType tlt) tps	-- mapM gives Nothing if one type is not found
			return $ cons rtps



superTypesFor	::  TypeTable -> TypeID -> [([RType],[Name], Map Name [RType])]
superTypesFor tt t
		= let	stfor		= findWithDefault M.empty t $ supertypes tt
			freeKeys	= keys stfor		in
			fmap (superTypesFor' stfor) freeKeys


superTypesFor'	:: SuperTypeTableFor -> [Name] -> ([RType],[Name], Map Name [RType])
superTypesFor' sttf appliedFrees
		= let	(supers, reqs)	= findWithDefault (S.empty,M.empty) appliedFrees sttf in
			(S.toList supers, appliedFrees, reqs)



showTLT dict	=  intercalate "; " $  fmap sitem $ M.toList dict

sitem (k, possib)	= spth k ++ " --> {"++intercalate ", " (fmap shwFQN possib)  ++ "}"
shwFQN (fqn, _)	= intercalate "." $ modulePath fqn
spth (nms, nm)	= intercalate "." $ nms ++ [nm]

showTypeID (fqn,nm)
		= show fqn ++"."++show nm
