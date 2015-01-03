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
import Languate.World

import Control.Arrow



type RequirementTable	= Map (TypeID, Int) (Set RType)
{- The type requirement table is the table which keeps track of what frees should be instances of what.

E.g.

data Bla (a:Eq) (b:Ord,Eq)	= Bla {a --> Sorted {b}}

Type requirements should be explicit in type declarations.
Mentally keeping track of where each requirement comes from is not user-friendly.
-}

buildRequirementTables	:: Map FQN TypeLookupTable -> World -> Exceptions' String (Map FQN RequirementTable)
buildRequirementTables tlts w
			= do	tables <- mapM (\(fqn, mod) -> buildRequirementTable tlts fqn (statements mod)) $ M.toList $ modules w
				return $ M.fromList tables

-- Builds the requirements table for all the type declarations in FQN
buildRequirementTable	:: Map FQN TypeLookupTable -> FQN -> [Statement] -> Exceptions' String (FQN, Map (TypeID, Int) (Set RType))
buildRequirementTable tlts fqn stmts
		= do	tlt	<- M.lookup fqn tlts ? ("Bug: no tlt found for "++show fqn++" while building typeReqTable")
			allReqs	<- mapM (requirementsIn tlt fqn) stmts |> concat
			return (fqn, M.fromList allReqs)


requirementsIn	:: TypeLookupTable -> FQN -> Statement -> Exceptions' String [((TypeID, Int), Set RType)]
requirementsIn tlt fqn (ADTDefStm (ADTDef name frees reqs _ _))
		=  buildReqs tlt fqn name frees reqs

requirementsIn tlt fqn (SynDefStm (SynDef name frees _ reqs))
		= buildReqs tlt fqn name frees reqs
requirementsIn tlt fqn (SubDefStm (SubDef name _ frees _ reqs ))
		= buildReqs tlt fqn name frees reqs
requirementsIn tlt fqn (ClassDefStm classDef)
		= buildReqs tlt fqn (name classDef) (frees classDef) (classReqs classDef)
requirementsIn _ _
		= return []

-- Build reqs in the frees for the definition of typeid
buildReqs	:: TypeLookupTable -> TypeID -> [Name] -> [TypeRequirement] -> [((TypeID, Int), Set RType)]
buildReqs tlt typeid frees reqs
		= let   reqTables 	= zip [0..] $ map (reqTable tlt reqs) frees :: [(Int, Set RType)] in
			map (\(argIndex, tps) -> ((typeid,argIndex), tps)) reqTables


reqTable	:: TypeLookupTable -> [TypeRequirement] -> Name -> Set RType
reqTable tlt reqs nm
		= S.fromList $ map (resolveType tlt . snd) $ filter ((==) nm . fst) reqs
