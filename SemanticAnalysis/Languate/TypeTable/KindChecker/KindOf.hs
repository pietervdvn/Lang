module Languate.TypeTable.KindChecker.KindOf where

import StdDef
import HumanUtils (pars)
import Exceptions
import Languate.CheckUtils
import qualified Data.Map as M
import Data.Map (Map, lookup)
import Prelude hiding (lookup)

import Languate.TAST
import Languate.TypeTable


{- given a rtype (of assumed kind), gives a binding or a fail if no biding is possible
Only RNormals, frees and applieds should be passed in;
Curries and Tuples bind only against 'Kind'
Assumes that, when hitting the bottom of a applied, you get the kind of the one in the KLT
-}
bindKind	:: KindLookupTable -> RType -> Kind -> Exc (Map Name Kind)
bindKind klt t k
	= do	let frees	= freesOf t
		let tid		= tidOf t
		bindKind' klt tid frees |> snd


{-
Calculates the binding of a TypeID applied to given frees.
-}
bindKind'	:: KindLookupTable -> TypeID -> [Name] -> Exc (Kind, Map Name Kind)
bindKind' klt tid frees
	= do	kind	<- lookup tid klt ? ("No kind found for "++show tid)
		_bk kind frees


appliedKinds	:: Kind -> [Kind]
appliedKinds Kind
		= []
appliedKinds (KindCurry k tail)
		= k : appliedKinds tail



_bk 		:: Kind -> [Name] -> Exc (Kind, Map Name Kind)
_bk k []	= return (k, M.empty)
_bk k@(KindCurry ka rest) (a:as)
	= inside ("In the binding of "++show k++" against "++show (a:as))	$ do
		(k, binding)	<- _bk rest as
		let (Just prevA)	= lookup a binding
		assert (a `M.notMember` binding || prevA == ka) $
			"Conflicting kinds for '"++a++"' found, both "++
			show ka++" and "++show prevA++" are possible."
		return (rest, M.insert a ka binding)
_bk Kind _
	= halt "Overapplication detected while binding the kinds"




freesOf	:: RType -> [Name]
freesOf (RApplied bt (RFree a))
	= freesOf bt ++ [a]
freesOf _
	= []


tidOf	:: RType -> TypeID
tidOf (RNormal fqn nm)
	= (fqn, nm)
tidOf (RApplied bt _)
	= tidOf bt
-- Unpacks applied types, returns the kind. Frees, Tuples and Curries return "Kind"
lookupBaseKind	:: KindLookupTable -> RType -> Exc Kind
lookupBaseKind klt t@(RNormal fqn n)
		= lookup (fqn, n) klt ? ("Bug: Type "++show t++" was not found in the KLT")
lookupBaseKind klt (RApplied bt _)
		= lookupBaseKind klt bt
lookupBaseKind _ _
		= return Kind






-- Gets the correct kind of a type (including applications), or an error message. Kinds might not be fully applied.
kindOf		:: KindLookupTable -> Map Name Kind -> RType -> Exc Kind
kindOf klt _ (RNormal fqn nm)
	= lookup (fqn, nm) klt ? ("Kind of the type "++show fqn++"."++nm++" was not found")
kindOf _ frees (RFree a)
	= lookup a frees ?
		("Free type variable '"++a++"' was not found.\n"++
			"Make sure it is declared before used (thus left of it's usage)")
kindOf klt frees t@(RApplied bt at)
	= inside ("In the kind calculation of "++show t) $ do
		bk	<- kindOf klt frees bt
		ak	<- kindOf klt frees at
		let msg	= "In the type application "++ pars (show t) ++" "++ pars (show at)
		inside msg $ applyKind t bk (ak, at)
kindOf klt frees t@(RCurry at rt)
	= inside ("In the kind calculation of "++show t) $ do
		let msg t k 	= "The type "++show t++" should be fully applied "++
					"as it is used in a curry, but it has the kind "++show k
		ak	<- kindOf klt frees at
		assert (ak == Kind) $ msg at ak
		rk	<- kindOf klt frees rt	-- calculate the kind of the rest, which should be fully applied too
		assert (rk == Kind) $ msg rt rk
		return Kind


-- Applies the first kind to given arguments. e.g. (* ~> (* ~> *) ~> *) $ [(* ~> *), (* ~> *)] is a valid application giving *
applyKind	:: RType -> Kind -> (Kind,RType) -> Exc Kind
applyKind baseType Kind _
	= do	err $ "Type overapplication:\n"++
			show baseType ++ " was applied to too much type arguments"
	 	return Kind
applyKind baseType bk@(KindCurry k rest) (argK,argT)
	= do	assert (sameStructure k argK) $
		    "Kind mismatch: could not unify "++show k++" and "++show argK++".\n"++
		    "The type "++show argT ++ "::" ++ show argK ++" was applied to "++show baseType ++ "::" ++ show bk
		return rest

sameStructure	:: Kind -> Kind -> Bool
sameStructure Kind Kind	= True
sameStructure (KindCurry k0 k1) (KindCurry k0' k1')
		= sameStructure k0 k0' && sameStructure k1 k1'
sameStructure _ _	= False
