module Languate.Typetable.TypeLookupTable.TypeLookupTableDef
	(TypeLookupTable, findTypeOrigin, findTypeOrigin', resolveType, tlt2doc) where

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


import Languate.AST
import Languate.Package
import Languate.TAST as TAST
import Languate.FQN

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


resolveTypeIn	:: TypeLookupTable -> (a,Type) -> Exc (a,RType)
resolveTypeIn tlt (a,tp)
	= do	rtp	<- resolveType tlt tp
		return (a,rtp)

resolveTypesIn	:: TypeLookupTable -> (a,[Type]) -> Exc (a,[RType])
resolveTypesIn tlt (a,tps)
	= do	rtps	<- mapM (resolveType tlt) tps
		return (a,rtps)

-- TODO are these functions used?
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


tlt2doc		:: String -> FQN -> TypeLookupTable -> Doc
tlt2doc title fqn tlt
		= let	mappings	= tlt |> S.toList & M.toList & unmerge |> swap & merge	:: [(FQN, [([Name],Name)])]
			mappings'	= mappings |||>>> (\(nms, nm) -> nms ++ [nm]) |||>>> intercal "." |> swap	:: [([String], FQN)]
			row (strings, fqn)	= [strings & sort |> code |> Parag & Mu.Seq, code $ show fqn] in
			doc (title ++ show fqn) ("What identifier does map on what type?") $
			table ["Allowed names", "Meaning"] (mappings' |> row)
spth (nms, nm)	= intercal "." $ nms ++ [nm]
