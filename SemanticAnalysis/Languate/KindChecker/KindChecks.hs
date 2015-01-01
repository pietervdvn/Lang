module Languate.KindChecker.KindChecks where

{--
This module implements various checks on kinds. It provides helper functions for the kindsolver.
--}

import StdDef
import MarkDown
import Exceptions
import Languate.Checks.CheckUtils

import Data.Map
import Prelude hiding (lookup)

import Languate.AST
import Languate.TAST
import Languate.TypeTable



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
		=  inside ("In the kind application of "++show rtype) $
			do	kind	<- kindOf klt frees rtype
				assert (0 == numberOfKindArgs kind) $ "Expecting " ++ number (numberOfKindArgs kind) ++ " more type arguments to "++show rtype



-- Gets the correct kind of a type (including applications), or an error message. Kinds might not be fully applied.
kindOf		:: KindLookupTable -> Map Name Kind -> RType -> Exceptions' String Kind
kindOf klt _ (RNormal fqn nm)
		= lookup (fqn, nm) klt ? ("Kind of the type "++show fqn++"."++nm++" was not found")
kindOf _ frees (RFree a)
		= return $ findWithDefault Kind a frees
kindOf klt frees (RApplied rt rts)
		= do	bk	<- kindOf klt frees rt
			appliedKnds	<- mapM (kindOf klt frees) rts
			applyKind (bk, rt, length appliedKnds) bk (zip appliedKnds rts)
kindOf klt frees (RCurry tps)
		= do	mapM_ (kindOf klt frees) tps	-- Recursively check kind applications, but throw result away
			return Kind
kindOf klt frees (RTuple tps)	= kindOf klt frees (RCurry tps)

-- Applies the first kind to given arguments. e.g. (* ~> (* ~> *) ~> *) $ [(* ~> *), (* ~> *)] is a valid application giving *
applyKind	:: (Kind, RType, Int) -> Kind -> [(Kind,RType)] -> Exceptions' String Kind
applyKind _ Kind []
		= return Kind
applyKind (origKind, rtype, provided) Kind args
		= do	err $ show rtype++" is applied to too many type arguments.\n"++
				show origKind++" takes only "++ plural (numberOfKindArgs origKind) "type argument" ++ ", but "++show provided++" were given."
		 	return Kind
applyKind ctx@(origKind, rtype, provided) (KindCurry k rest) ((arg,t):args)
		= do	assert (sameStructure k arg) $
				"Kind mismatch: could not unify "++show k++" and "++show arg++", which are the "++(count $ numberOfKindArgs origKind - numberOfKindArgs rest)++
				" needed argument of "++show rtype++" and the kind of "++show t
			applyKind ctx rest args
applyKind _ rest []
		= return rest

sameStructure	:: Kind -> Kind -> Bool
sameStructure Kind Kind	= True
sameStructure (KindCurry k0 k1) (KindCurry k0' k1')
		= sameStructure k0 k0' && sameStructure k1 k1'
sameStructure _ _	= False
