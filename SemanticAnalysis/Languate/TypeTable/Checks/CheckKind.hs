module Languate.TypeTable.Checks.CheckKind where

{--
This module implements various checks on kinds. It provides helper functions for the kindsolver.
--}

import StdDef
import MarkDown
import Exceptions
import Languate.CheckUtils

import Data.Map hiding (map, filter)
import Prelude hiding (lookup)
import Data.Set (Set)
import qualified Data.Set as S
import Data.List (intercalate)

import Languate.AST
import Languate.TAST
import Languate.TypeTable
import Languate.FQN
import Languate.TypeTable.KindChecker.KindConstraint
import Languate.TypeTable.KindChecker.KindOf

import Languate.Graphs.SearchCycles


{-Validates wether the 'HaveSameKind'-constraints are met-}
validateSameKindConstraints	:: KindLookupTable -> TypeReqTable -> ((RType, RType), Location) -> Check
validateSameKindConstraints klt treqt ((rt0, rt1),loc)
      = onLocation loc $ inside ("Kind constraint error on "++ st True rt0 ++ " and "++st True rt1) $ try err $ do
	bk0	<- lookupBaseKind klt rt0
	bk1	<- lookupBaseKind klt rt1
	-- TODO calculate the kind of the frees, found in the reqs of the type
	kinds1	<- _bindReqKinds klt treqt rt0
	kinds2	<- _bindReqKinds klt treqt rt1
	binding	<- bindKind klt rt0 bk0
	k0	<- kindOf klt binding rt0
	k1	<- kindOf klt binding rt1
	assert (k0 == k1) $ "The types "++ st True rt0++" and "++st True rt1++" should have the same kinds.\n"++
		st True rt0++"::"++show k0 ++ "\n"++
		st True rt1++"::"++show k1

-- calculates kinds for things bound via the type reqs
_bindReqKinds	:: KindLookupTable -> TypeReqTable -> RType -> Exc (Map Name Kind)
_bindReqKinds klt treqt rt
		= do	mtid(Just tid)	= getBaseTID rt
			when (isJust tid) $ do
			kind	<- lookup tid klt ? "No klt-entry for "++show tid
			nrFrees	= numberOfKindArgs kind
			-- we now have the actual requirements for each type var of rt
			frees	= [0..nrFrees] |> (\i -> findWithDefault S.empty i treqt)
			-- we bind to each index the kind of the type variable
			kindOf	= appliedKinds kind
			binds	= mapM (uncurry $ bindKind klt) $ zip frees kindOf




{-Code to report cycles-}
reportCycles	:: Map (FQN, Name) (Set (FQN, Name)) -> Check
reportCycles graph
		= let	cycles	= cleanCycles graph in
			mapM_ reportCycle cycles

filterCycles	:: [(FQN, Name)] -> [SimpleConstraint'] -> [SimpleConstraint']
filterCycles inCycle
		= filter (\((id,_),_) -> id `notElem` inCycle)

reportCycle	:: [(FQN, Name)] -> Check
reportCycle [a,_]
		= err $ "Could not construct the infinite type "++showTypeID a++". Please remove the dependency (in the type requirements) on itself."
reportCycle (typ:path)
		= err $ indent' ("Could not construct the kind for "++ showTypeID typ ++", as this type has a cyclic dependency") $
				intercalate " -> " $ map showTypeID path

cyclesIn	:: [SimpleConstraint] -> Map (FQN, Name) (Set (FQN, Name))
cyclesIn	=  searchCycles . fromList . map buildDeps


buildDeps	:: SimpleConstraint -> ((FQN, Name), (Set (FQN, Name)))
buildDeps (id, uk)
		= (id, S.fromList $ dependsOn uk)



{-Over application checks -}

reportOverApplication	:: KindLookupTable -> SimpleConstraint' -> Check
reportOverApplication klt (((fqn, nm), uk), coor)
		= inFile fqn $ onLine coor $ checkUKOverApp klt uk

checkUKOverApp	:: KindLookupTable -> UnresolvedKind -> Check
checkUKOverApp _ UKind	= pass
checkUKOverApp klt (UKindCurry k1 k2)
			= checkUKOverApp klt k1 >> checkUKOverApp klt k2
checkUKOverApp klt (SameAs rtype)
			= do	kindOf klt empty rtype
				return ()


{- Checks types are correctly applied. E.g. ''Functor Int Int'' does not make sense, just as Functor -> Monad.

Checks performed:
- Over application
- Wrong Kind application (e.g. expected a type of Kind (* ~> *), but one of a different kind is given)
- Under application (if you want to get the kind of a partially applied type function, use ''kindOf'')

Not checked here/Assumptions
- Undeclared frees, all frees should be declared at this point. (Frees not provided in the table will be treated as having kind '*')

Args:
- KLT
- Free map. Gives the kind of the known frees, e.g. {a --> "*", c --> "* ~> *"}
- Applied type, e.g. "Functor a String b" (obviously fualty)
-}
checkKindApp	:: KindLookupTable -> Map Name Kind -> RType -> Check
checkKindApp klt frees rtype
		=  inside ("In the kind application of "++st True rtype) $
			do	kind	<- kindOf klt frees rtype
				assert (0 == numberOfKindArgs kind) $ "Expecting " ++ number (numberOfKindArgs kind) ++ " more type arguments to "++st True rtype
