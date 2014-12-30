module Languate.TypeTable.BuildRequirementTable where

{--
Gives the type requirements for each type.
--}

import StdDef
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Map (Map, mapWithKey, findWithDefault)
import Data.Set (Set)

import Languate.AST
import Languate.TAST
import Languate.FQN
import Languate.TypeTable
import Languate.World

import Control.Arrow


type RequirementTable	= Map (TypeID, Int) (Set RType)
{- The type requirement table is the table which keeps track of what frees should be instances of what.

E.g.

data Bla (a:Eq) (b:Ord,Eq)	= Bla {a --> Sorted {b}}

Type requirements should be explicit in type declarations.
Mentally keeping track of where each requirement comes from is not user-friendly.
-}

buildRequirementTables	:: Map FQN TypeLookupTable -> World -> Map FQN RequirementTable
buildRequirementTables tlts w
			= let	mods	= modules w
				lookup' fqn	= findWithDefault (error $ "Bug: No tlt for FQN "++show fqn) fqn tlts in
				mapWithKey (\fqn mod -> buildRequirementTable (lookup' fqn) (statements mod)) mods

buildRequirementTable	:: TypeLookupTable -> [Statement] -> Map (TypeID, Int) (Set RType)
buildRequirementTable tlt stmts
		= M.unions $ map (M.fromList . requirementsIn tlt) stmts


requirementsIn	:: TypeLookupTable -> Statement -> [((TypeID, Int), Set RType)]
requirementsIn tlt (ADTDefStm (ADTDef name frees reqs _ _))
		=  buildReqs tlt name frees reqs

requirementsIn tlt (SynDefStm (SynDef name frees _ reqs))
		= buildReqs tlt name frees reqs
requirementsIn tlt (SubDefStm (SubDef name _ frees _ reqs ))
		= buildReqs tlt name frees reqs
requirementsIn tlt (ClassDefStm classDef)
		= buildReqs tlt (name classDef) (frees classDef) (classReqs classDef)
requirementsIn _ _
		= []

buildReqs	:: TypeLookupTable -> Name -> [Name] -> [TypeRequirement] -> [((TypeID, Int), Set RType)]
buildReqs tlt name frees reqs
		= let 	fqn	= _resolveType' tlt ([], name)
			typeid	= (fqn, name)
			reqTables = zip [0..] $ map (reqTable tlt reqs) frees :: [(Int, Set RType)] in
			map (\(argIndex, tps) -> ((typeid,argIndex), tps)) reqTables


reqTable	:: TypeLookupTable -> [TypeRequirement] -> Name -> Set RType
reqTable tlt reqs nm
		= S.fromList $ map (resolveType tlt . snd) $ filter ((==) nm . fst) reqs
