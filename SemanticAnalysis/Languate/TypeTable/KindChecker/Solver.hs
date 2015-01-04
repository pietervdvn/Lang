module Languate.TypeTable.KindChecker.Solver where

{--

Kind constraints come in, kinds for types come out!

--}

import StdDef
import State
import Exceptions

import Data.Char
import Data.Maybe
import Data.Set (Set)
import Data.List (intercalate)
import qualified Data.Set as S
import Data.Map (Map, elems, empty, lookup, insert, fromList, keys, empty)
import Prelude hiding (lookup, iterate)
import Control.Monad
import Control.Arrow

import Languate.FQN
import Languate.AST (Coor)
import Languate.TAST
import Languate.TypeTable.KindChecker.KindConstraint
import Languate.TypeTable.KindChecker.KindChecks
import Languate.Checks.CheckUtils
import Languate.TypeTable


{-

Solves the type constraints.
We make a few assumptions:

Type requirements are all given withing the constraints. This means we have free kind arguments, but means that "have same kind as" is really the same kind, and a (*) can not be expanded into (* -> *)


-}

solve		:: TypeReqTable -> Map TypeID (Map Int Name) -> [(KindConstraint, Location)] -> Exc KindLookupTable
solve treqt freenmt allConstr
		= inside ("While building the kind table") $ do
			klt 	<- solveAll $ mapMaybe (unpackMaybeTuple . (first getHasKind)) allConstr
			let sameKindConstraints	= map (first haveSameKinds) allConstr
			let sameKinds	= mapMaybe unpackMaybeTuple sameKindConstraints
			-- TODO fix frees! issue #57
			mapM_ (validateSameKindConstraints klt empty) sameKinds
			return klt


solveAll	:: [SimpleConstraint'] -> Exc KindLookupTable
solveAll constraints
		= do	let (failed, klt) = runstate (solveAll' constraints) empty
			let cycles	= cyclesIn $ map fst failed
			reportCycles cycles
			mapM_ (reportOverApplication klt) $ filterCycles (keys cycles) failed
			return klt

solveAll' 	:: [SimpleConstraint'] -> State KindLookupTable [SimpleConstraint']
solveAll' []	=  return []
solveAll' constrs
		= do	failed	<- mapM addConstraint constrs |> catMaybes
			if length failed == (length constrs)
				then return failed
				else solveAll' failed	-- progress has been made, we retry the failed constraints. Some might be solvable now!



-- Resolves the kinds of the embedded types and adds to the table. If a dependency is still missing or over applied, the constraint is returned
addConstraint	:: SimpleConstraint' -> State KindLookupTable (Maybe SimpleConstraint')
addConstraint sc@(((fqn, n), uk), _)
		= do	klt	<- get
			maybe (return $ Just sc)
				(\k -> do	addKind (fqn, n) k
						return Nothing)
				(resolveKind klt uk)



addKind		:: (FQN, Name) -> Kind -> State KindLookupTable ()
addKind key kind	= modify (insert key kind)


-- lookups the kind in the table with 'same kind as'
resolveKind	:: KindLookupTable -> UnresolvedKind -> Maybe Kind
resolveKind _ UKind	= return Kind
resolveKind klt (UKindCurry uk1 uk2)
			= do	k1	<- resolveKind klt uk1
				k2	<- resolveKind klt uk2
				return $ KindCurry k1 k2
resolveKind klt (SameAs rtype)
			= _kindOf klt rtype


-- simple kind application. kindOf Functor = "* ~> *", kindOf (Functor a) = "*". No recursive checks: e.g. kindOf Functor (Monad) = "*", the fact that monad is not applied enough does not matter
_kindOf		:: KindLookupTable -> RType -> Maybe Kind
_kindOf klt (RNormal fqn nm)
		=  lookup (fqn, nm) klt
_kindOf klt applied@(RApplied rtype rts)
		= do	baseKind	<- _kindOf klt rtype
			simpleKindApply applied baseKind
_kindOf _ _	= Just Kind



simpleKindApply	:: RType -> Kind -> Maybe Kind
simpleKindApply (RApplied rtype []) kind
		= Just kind
simpleKindApply (RApplied _ _) Kind
		= Nothing	-- over application. Returns nothing
simpleKindApply (RApplied rtype (rt:rts)) (KindCurry _ rest)
		= simpleKindApply (RApplied rtype rts) rest
simpleKindApply _ _
		= Nothing
