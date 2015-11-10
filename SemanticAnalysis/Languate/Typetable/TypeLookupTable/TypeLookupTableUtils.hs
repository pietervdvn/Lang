module Languate.Typetable.TypeLookupTable.TypeLookupTableUtils where

{--
This module implements the actual querying functions
--}

import StdDef
import Exceptions
import Languate.CheckUtils
import HumanUtils
import Languate.MarkUp as Mu

import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Tuple
import Data.List (sort)

import Languate.Typetable.TypeLookupTable.TypeLookupTableDef
import Languate.AST
import Languate.Package
import Languate.TAST as TAST
import Languate.FQN


knownTypes	:: TypeLookupTable -> [TypeID]
knownTypes tlt
	= tlt |> S.toList & M.toList & unmerge |> swap ||>> snd


-- Finds the type within the TLT
resolveTypeOrigin	:: TypeLookupTable -> ([Name], Name) -> Exc FQN
resolveTypeOrigin tlt id@(path, t)
	=  do	fqnSet	<- M.lookup id tlt ? ("The type "++spth id ++" could not be resolved. It is not declared or imported.")
		let fqns	= S.toList fqnSet
		assert (isSingleton fqns) $ "The type "++spth id++" is ambigous. We found it in modules "++show fqns
		return $ head fqns

resolveTypeID	:: TypeLookupTable -> ([Name], Name) -> Exc (FQN, Name)
resolveTypeID tlt path@(_,nm)
		= do	fqn	<- resolveTypeOrigin tlt path
			return (fqn, nm)


resolveTypePath	:: TypeLookupTable -> ([Name],Name) -> Exc RType
resolveTypePath tlt id@(_, nm)
		= do	fqn	<- resolveTypeOrigin tlt id
			return $ RNormal fqn nm


resolveType	:: TypeLookupTable -> Type -> Exc RType
resolveType tlt (Normal path name)
		= do	fqn	<- resolveTypeOrigin tlt (path, name)
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
resolveType tlt (TupleType [])
		= return voidType
resolveType tlt (TupleType [t])
		= resolveType tlt t
resolveType tlt e@(TupleType (t:tps))
		= do	rt	<- resolveType tlt t
			tail	<- resolveType tlt (TupleType tps)
			return $ RApplied (RApplied tupleType rt) tail
resolveType _ DontCareType
		= halt "Unresolved dont care type"


resolveTypes tlt
		= mapM (resolveType tlt)

resolveReqs	:: TypeLookupTable -> [TypeRequirement] -> Exc RTypeReq
resolveReqs tlt reqs
		= reqs & merge |> onSecond (resolveTypes tlt) & sequence


spth (nms, nm)	= intercal "." $ nms ++ [nm]
