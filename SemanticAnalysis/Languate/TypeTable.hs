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
import Languate.TAST as TAST
import Languate.FQN
import Languate.CheckUtils

import Data.Maybe
import Data.List as L

import Control.Arrow
import Normalizable

{-
The type table contains all known types within a certain module.
-}



{-Type requirements are explicit for new type declarations; in a "(freeName, reqs)" format, where each subsequent free is named "a0","a1",...

Might contain synonyms
-}
type TypeReqTable	= Map TypeID [(Name, Set RType)]
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
A "FullSuperTypeTable" is saved for each typeId. It represents "This type is A if these conditions are met."

E.g. for list a:
Mappable a	--> []	-- no conditions on the frees
Collection a	--> []	-- idem
Monoid 		--> []-- thus: if applied to a single free (with no special requirements)
Dict k v	--> [a is "(k,v)"]	-- It is a dict if applied to a tuple

-- This table gets built recursively via the already known supertypes:
-- added via collection:

Eq --> ["Eq"]	-- List Eq is Eq
-- Monoid		--> [{}]	-- but already in the table


E.g. For "Weighted" (graph)
We know that the first free should be a "Graph", thus:
Graph n		--> [{graph, Graph}, {n, Ord, Eq}, {w, Monoid, Ord, Eq}, {n}]


The binding maps free type variables from the *supertype* (given) to *subtype*
-}
data FSTTEntry	= FSTTEntry {	reqs		:: [(Name,Set RType)],	-- The requiments needed to be this type
				viaType		:: Maybe RType, 	-- The supertype of T which caused the current supertype to be added (and is in the list)
				origSuper	:: RType,		-- The type this super was derived from, e.g. Dict a0 a1
				origBinding		:: Binding,		-- The binding which takes the orig super into it's actual form, e.g. Dict k1 v1
				stepBinding	:: Maybe Binding	-- The binding which takes the frees to this form
				}
	deriving (Show, Eq, Ord)
type FSTTKeyEntry	= (RType, FSTTEntry)
type FullSuperTypeTable	= Map RType FSTTEntry
type FullSuperTypeTables	= Map TypeID FullSuperTypeTable
{-

Aid while building the FSTT:
It saves what supers exist in what form,e.g. for list

Dict --> [""Dict k1 v1"", ""Dict k1 [v1]""]
Set  --> [""Set a0""]
-}
type SpareSuperTypeTable	= Map TypeID [RType]
type SpareSuperTypeTables	= Map TypeID SpareSuperTypeTable

data TypeTable	= TypeTable	{ knownTypes	:: Set TypeID
				, typeLookups	:: Map FQN TypeLookupTable
				, kinds		:: KindLookupTable
				, typeReqs	:: TypeReqTable
				, supertypes	:: Map TypeID SuperTypeTableFor
				, allSupertypes	:: Map TypeID FullSuperTypeTable
				, spareSuperTypes	:: Map TypeID SpareSuperTypeTable
				, docstrings	:: Map TypeID String
				, freeNames	:: Map TypeID (Map Int Name)}
	deriving (Show, Ord, Eq)


data Binding	= Binding (Map Name RType)	-- data type, and not a type alias to allow a custom show function
	deriving (Eq, Ord)

{-
Constructs the vanillaType of the given ID.
The vanillaType is the type with frees, e.g.
Dict a0 a1
-}
vanillaType'	:: KindLookupTable -> TypeID -> RType
vanillaType' klt tid@(fqn, nm)
	= let	nrOfArgs	= findWithDefault Kind tid klt & numberOfKindArgs
		frees		= defaultFreeNames & take nrOfArgs |> RFree	in
		Prelude.foldl RApplied (RNormal fqn nm) frees

-- same a vanillaType', except this one fetches the KLT from the TT
vanillaType	:: TypeTable -> TypeID -> RType
vanillaType tt
		= vanillaType' (kinds tt)


{-
How much arguments takes this type?
e.g.
""Int""	0
(function with one argument)
""Int -> Int"" 1
""Associative a" takes 2 arguments
-}

curryNumber	:: TypeTable -> RType -> Int
curryNumber tt	= _curryNumber tt []


_curryNumber	:: TypeTable -> [RType] -> RType -> Int
_curryNumber tt _ (RCurry _ b)
		= 1 + curryNumber tt b
_curryNumber tt visited rt
 | isNormal rt	= _curryNumber' tt (rt:visited) $ fromJust $ getBaseTID rt
 | otherwise	= 0 -- TODO error $ "Curry numbers do not make sense for free type variables"

curryNumber'	:: TypeTable -> TypeID -> Int
curryNumber' tt	= _curryNumber' tt []

_curryNumber'	:: TypeTable -> [RType] -> TypeID -> Int
_curryNumber' tt visited tid
 | tid == anyTypeID	= 0
 | otherwise	= let	errMsg	= "No super type table for "++show tid++", weird..."
			superTT	= tt & allSupertypes & findWithDefault (error errMsg) tid  in
			if anyType `M.member`  superTT then 0 else
			let super	= head $ L.filter (`notElem` visited) $
						M.keys superTT in
			_curryNumber tt visited super

-- Returns all the supertypes, given per applied frees. Whenever the type is fully applied, ''Any'' is a supertype too. This supertypes is given when no other supertype exists. Used in the 2MD
superTypesFor	::  TypeTable -> TypeID -> [([Name], RType, Map Name [RType])]
superTypesFor tt t
		= let	stfor		= findWithDefault M.empty t $ supertypes tt
			freeKeys	= keys stfor		in
			concatMap (superTypesFor' stfor) freeKeys


superTypesFor'	:: SuperTypeTableFor -> [Name] -> [([Name], RType, Map Name [RType])]
superTypesFor' sttf appliedFrees
		= let	all	= S.toList $ findWithDefault S.empty appliedFrees sttf	:: [(RType, Map Name [RType])] in
			unmerge [(appliedFrees , all)] |>  (\(a,(b,c)) -> (a,b,c))




instance Show Binding where
	show	= sb

sb (Binding b)
	= sd b


sd	:: (Show k, Show v) => Map k v -> String
sd  d
	= "{"++unwords (M.toList d |> (\(k, v) -> show k ++" --> "++show v))++"}"

noBinding	= Binding M.empty
