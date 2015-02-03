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
import Languate.CheckUtils

import Control.Arrow
import Normalizable
{-
The type table contains all known types within a certain module.
-}

type TypeID	= (FQN, Name)

--Type requirements are explicit for new type declarations; contains synonyms
type TypeReqTable	= Map (TypeID, Int) (Set RType)
type KindLookupTable	= Map TypeID Kind

{-
Keeps track of supertype relations.
These are direct super types. All should have the same kind. E.g. String in List Char; both are *

E.g.

instance List (a:Eq) is Eq
instance List (a:Eq) is Set a
instance Dict k (v:Dict k k) is Multigraph

This is saved as
{List --> { [] -> Collection
	    ["a"] --> { Eq	, where "a:Eq"}
	    }

This table is only a intermediate structure (which is usefull to generate the docs), but is transformed further into a 'recursiveSTT', which grants easier access.
-}

type SuperTypeTableFor	= Map [Name] (Set (RType, Map Name [RType]))
type SuperTypeTable	= Map TypeID SuperTypeTableFor


{-
Encodes the supertype relationship, but with a 'met type requirement approach'.

E.g.
List	is Mappable
List a	is Collection a	=> gets written as List is Collection
List Char is Show
List Eq is Eq

The 'typeId' points to a 'root' recursive table
List -> isA {Mappable, Collection}, recursiveReqs: { (a,[Eq]) --> isA {Eq}, {} ; (a,[Char]) --> isA {Show}, {}}

The 'Name' in the tuple of the keys denote what the name of the bound out free was

Note that type application makes only sense on
-> Other applied types
-> Frees (a b, where 'a' has e.g. the requirements "collection, mappable")
-> Normals

-}
data RecursiveSuperTypeTable	= RTT { isA :: Set RType, recursiveReqs	:: Map (Name, Set RType) RecursiveSuperTypeTable }
	deriving (Eq, Ord)


-- The (implicit) supertype for every type
anyType		= uncurry RNormal anyTypeID
anyTypeID	= (toFQN' "pietervdvn:Data:Any", "Any")

-- basically the same as the aliastable, but with types.
type TypeLookupTable	= Map ([Name], Name) (Set FQN)	-- mutliple values, multiple possiblities in some cases!





data TypeTable	= TypeTable	{ typeLookups	:: Map FQN TypeLookupTable
				, kinds		:: KindLookupTable
				, typeReqs	:: TypeReqTable
				, supertypes	:: Map TypeID SuperTypeTableFor
				, recSupertypes	:: Map TypeID RecursiveSuperTypeTable
				, docstrings	:: Map TypeID String
				, freeNames	:: Map TypeID (Map Int Name)}
	deriving (Show, Ord, Eq)








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
resolveType tlt (Applied bt [])
		= resolveType tlt bt
resolveType tlt (Applied bt tps)
		= do	let (tps',t)	= (init' tps, last tps)
			t'	<- resolveType tlt t
			tail	<- resolveType tlt (Applied bt tps')
			return $ RApplied tail t'

resolveType tlt (Curry [t])
		= resolveType tlt t
resolveType tlt (Curry (t:tps))
		= do	t'	<- resolveType tlt t
			tail	<- resolveType tlt (Curry tps)
			return $ RCurry t' tail
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

_construct'	:: TypeLookupTable -> Type -> Type -> Type -> (RType -> RType -> RType) -> Exc RType
_construct' tlt e t0 t1 cons
		= inside ("In the type expression "++show e) $ do
			t0'	<- resolveType tlt t0
			t1'	<- resolveType tlt t1
			return $ cons t0' t1'

-- Returns all the supertypes, given per applied frees. Whenever the type is fully applied, ''Any'' is a supertype too. This supertypes is given when no other supertype exists. Used in the 2MD
superTypesFor	::  TypeTable -> TypeID -> [([Name], RType, Map Name [RType])]
superTypesFor tt t
		= let	stfor		= findWithDefault M.empty t $ supertypes tt
			freeKeys	= keys stfor		in
			concatMap (superTypesFor' stfor) freeKeys


superTypesFor'	:: SuperTypeTableFor -> [Name] -> [([Name], RType, Map Name [RType])]
superTypesFor' sttf appliedFrees
		= let	all	= S.toList $ findWithDefault S.empty appliedFrees sttf	:: [(RType, Map Name [RType])] in
			fmap (\(a,(b,c)) -> (a,b,c)) $ unmerge [(appliedFrees , all)]


showTLT dict	=  intercalate "; " $  fmap sitem $ M.toList dict

sitem (k, possib)	= spth k ++ " --> {"++intercalate ", " (fmap shwFQN possib)  ++ "}"
shwFQN (fqn, _)	= intercalate "." $ modulePath fqn
spth (nms, nm)	= intercalate "." $ nms ++ [nm]

showTypeID (fqn,nm)
		= show fqn ++"."++show nm

instance Show RecursiveSuperTypeTable where
	show	= srstt 0


srstt	:: Int -> RecursiveSuperTypeTable -> String
srstt indent rstt
	= let	isa	= "{{" ++ intercalate ", " (S.toList (isA rstt) |> st True ) ++ "}"
		showEntry (name, reqs) rstt	= name ++ ": " ++ (intercalate ", " (S.toList reqs |> st True)) ++ " --> "++srstt (indent + 3) rstt
		entries	= fmap (uncurry showEntry) $ M.toList $ recursiveReqs rstt in
		(intercalate ("\n" ++ replicate indent ' ' ) $ isa : entries) ++ "}"
