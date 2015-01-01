module Languate.KindChecker.Solver where

{--

Kind constraints come in, kinds for types come out!

--}

import StdDef
import State
import Exceptions

import Data.Char
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map, elems, empty, lookup, insert, fromList)
import Prelude hiding (lookup, iterate)
import Control.Monad
import Control.Arrow

import Languate.Graphs.SearchCycles

import Languate.FQN
import Languate.AST (Coor)
import Languate.TAST
import Languate.KindChecker.KindConstraint
import Languate.KindChecker.KindChecks
import Languate.Checks.CheckUtils
import Languate.TypeTable


{-

Solves the type constraints.
We make a few assumptions:

Type requirements are all given withing the constraints. This means we have free kind arguments, but means that "have same kind as" is really the same kind, and a (*) can not be expanded into (* -> *)


-}

type SimpleConstraint	= ((FQN, Name), UnresolvedKind)
type SimpleConstraint'	= (SimpleConstraint, Coor)

solve		:: Map FQN [(KindConstraint, Coor)] -> Exceptions' String KindLookupTable
solve dict	=  let constrs	= filter (isHasKind .fst) $ concat $ elems dict	in
			stack' ("While building the kind table:\n"++) $
				solveAll $ map (first $ \(HasKind id uk) -> (id,uk)) constrs

solveAll	:: [SimpleConstraint'] -> Exceptions' String KindLookupTable
solveAll constraints
		= do	let (failed, klt) = runstate (solveAll' constraints) empty
			warn $ "Following constraints were not resolved: "++show failed	-- TODO remove warning when debug fase is done
			let cycles	= cyclesIn $ map fst failed
			err $ "Cycles found: "++show cycles
			-- mapM_ (validateConstraint klt) failed
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

{- Validates all the SAME AS in the kind constraint. Assumes all kinds are known within the klt
validateConstraint	:: KindLookupTable -> SimpleConstraint' -> Check
validateConstraint klt (((fqn, name), uk), coor)
	= do	let frees	= buildFreeTable klt uk 0
		inFile fqn $ onLine coor $ inside ("In the type declaration of "++name) $
			validateUK klt frees uk


validateUK	:: KindLookupTable -> Map Name Kind -> UnresolvedKind -> Check
validateUK klt frees (SameAs rt)
		= checkKindApp klt frees rt
validateUK klt frees (UKindCurry k1 k2)
		= do	validateUK klt frees k1
			validateUK klt frees k2
validateUK _ _ _= pass


buildFreeTable	:: KindLookupTable -> UnresolvedKind -> Int -> Exceptions' String (Map Name Kind)
buildFreeTable klt UKind _	= return empty
buildFreeTable klt (UKindCurry UKind rest) index
		= buildFreeTable klt rest (index + 1)
buildFreeTable klt (SameAs rtype)
		= do	kindOf klt

-}


cyclesIn	:: [SimpleConstraint] -> Map (FQN, Name) (Set (FQN, Name))
cyclesIn	=  searchCycles . fromList . map buildDeps


buildDeps	:: SimpleConstraint -> ((FQN, Name), (Set (FQN, Name)))
buildDeps (id, uk)
		= (id, S.fromList $ dependsOn uk)
