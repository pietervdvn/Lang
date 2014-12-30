module Languate.KindChecker.Solver where

{--

Kind constraints come in, kinds for types come out!

--}

import StdDef
import State
import Data.Maybe
import Data.Map (Map, elems, empty, lookup, insert)
import Prelude hiding (lookup, iterate)
import Control.Monad

import Languate.FQN
import Languate.TAST
import Languate.KindChecker.KindConstraint


type KindLookupTable	= Map (FQN, Name) Kind


solve	:: Map FQN [KindConstraint] -> Map (FQN, Name) Kind
solve d	=  let	constraints	= concat $ elems d
		mostArgs	= maximum $ mapMaybe numberOfArgs' constraints in
		snd $ runstate (mapM_ (iterate constraints) [0..mostArgs]) empty

-- A iteration solves all kind constraints with exactly i arguments, and uses solved things with less typeargs
iterate	:: [KindConstraint] -> Int -> State KindLookupTable ()
iterate constraints i
	=  do	solveUnder constraints i

-- Adds all kinds with exactly i kind arguments
solveUnder	:: [KindConstraint] -> Int -> State KindLookupTable ()
solveUnder constraints args
		= mapM_ (addConstrUnder args) constraints

addConstrUnder	:: Int -> KindConstraint -> State KindLookupTable ()
addConstrUnder i (HasKind (RNormal fqn nm) ukind)
		= do	let nArgs	= numberOfArgs ukind
			let exactArgs	= maybe False (\n -> n == i) nArgs
			when exactArgs $ addKind (fqn, nm) ukind
addConstrUnder _ _	= return ()


addKind		:: (FQN, Name) -> UnresolvedKind -> State KindLookupTable ()
addKind key ukind
		= do	maybeKind	<- embed $ flip resolveKind ukind
			maybe (return ())(\kind -> modify (insert key kind)) maybeKind


-- lookups the kind in the table with 'same kind as'
resolveKind	:: KindLookupTable -> UnresolvedKind -> Maybe Kind
resolveKind _ UKind	= return Kind
resolveKind klt (UKindCurry uk1 uk2)
			= do	k1	<- resolveKind klt uk1
				k2	<- resolveKind klt uk2
				return $ KindCurry k1 k2
resolveKind klt (SameAs (RNormal fqn name))
			= lookup (fqn, name) klt
resolveKind _ _		= Nothing
