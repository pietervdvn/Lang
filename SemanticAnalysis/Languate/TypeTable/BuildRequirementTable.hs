module Languate.TypeTable.BuildRequirementTable where

{--
Gives the type requirements for each type.
--}

import StdDef
import Exceptions
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Map (Map, mapWithKey, findWithDefault)
import Data.Set (Set)

import Languate.AST
import Languate.TAST
import Languate.FQN
import Languate.TypeTable
import Languate.Package

import Control.Arrow

import Languate.CheckUtils


{- The type requirement table is the table which keeps track of what frees should be instances of what.

E.g.

data Bla (a:Eq) (b:Ord,Eq)	= {Bla --> ["a0", {"Eq"}; "a1", {"Ord","Eq"} ]}

-}

buildRequirementTable	:: Package -> Map FQN TypeLookupTable -> Set TypeID
				-> Exc TypeReqTable
buildRequirementTable package tlts knownTypes
 = do	rawReqs	<- mapM (uncurry $ buildRequirementTableFor tlts) $ M.toList $ modules package
	rawReqs	& concat & merge ||>> S.unions
		|> (\((tid, freeIndex), req) -> (tid,('a':show freeIndex, req)))
		& merge & M.fromList & makeComplete knownTypes & return

-- Adds types to the trt with no supertypes, so that missing types in trt don't happen
makeComplete	:: Set TypeID -> TypeReqTable -> TypeReqTable
makeComplete known trt
	= let	missing	= S.filter (not . flip M.member trt) known & S.toList
		extras	= zip missing (repeat []) & M.fromList in
		M.union trt extras




buildRequirementTableFor	:: Map FQN TypeLookupTable -> FQN -> Module ->
					Exc [((TypeID, Int), Set RType)]
buildRequirementTableFor tlts fqn m
	= do	tlt	<- M.lookup fqn tlts ? ("No tlt for "++show fqn)
		rawReqs	<- mapM (requirementsIn tlt fqn) (statements m)
		return $ rawReqs & concat & merge ||>> S.unions



requirementsIn	:: TypeLookupTable -> FQN -> Statement -> Exc [((TypeID, Int), Set RType)]
requirementsIn tlt fqn (ADTDefStm (ADTDef name frees reqs _))
		=  buildReqs tlt (fqn, name) frees reqs

requirementsIn tlt fqn (SynDefStm (SynDef name frees _ reqs))
		= buildReqs tlt (fqn, name) frees reqs
requirementsIn tlt fqn (SubDefStm (SubDef name _ frees _ reqs))
		= buildReqs tlt (fqn, name) frees reqs
requirementsIn tlt fqn (ClassDefStm classDef)
		= buildReqs tlt (fqn, name classDef) (frees classDef) (classReqs classDef)
requirementsIn _ _ _
		= return []

-- Build reqs in the frees for the definition of typeid
buildReqs	:: TypeLookupTable -> TypeID -> [Name] -> [TypeRequirement] -> Exc [((TypeID, Int), Set RType)]
buildReqs tlt typeid frees reqs
		= do	reqs'		<- mapM (resolveReq tlt) reqs
			let reqTables	= zip [0..] $ map (reqTable reqs') frees :: [(Int, Set RType)]
			let addIndices (argIndex, tps)	= ((typeid,argIndex), tps)
			return $ map addIndices reqTables


reqTable	:: [(Name, RType)] -> Name -> Set RType
reqTable reqs nm
		= S.fromList $ map snd $ filter ((==) nm . fst) reqs

resolveReq	:: TypeLookupTable -> TypeRequirement -> Exc (Name, RType)
resolveReq tlt (nm, typ)
		= do	rtype	<- resolveType tlt typ
			return (nm, rtype)
