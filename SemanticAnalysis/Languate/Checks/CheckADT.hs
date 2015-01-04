module Languate.Checks.CheckADT where

{--
This module implements a full ADT-check
--}

import StdDef
import Exceptions
import Languate.Checks.CheckUtils
import Languate.Checks.CheckComment
import Languate.Checks.CheckType

import Languate.AST
import Languate.TypeTable
import Data.Maybe
import Data.List

import Control.Arrow


validateADTDef tlt (ADTDef nm frees reqs docStr sums)
		= inside ("In the definition of ADT-type "++show nm) $
		   do	validateComment' docStr
			validateReqs tlt frees reqs
			validateReqsFreeOrder reqs frees
			mapM_ (validateADTSum tlt frees) sums
			let cons = map (\(ADTSum nm _ _ _) -> nm) sums
			crossValidate tlt frees sums
			assert (unique cons) $ "A constructor name should be unique. You used "++ intercalate ", " (dubbles cons) ++ " at least twice"


validateADTSum tlt frees (ADTSum nm _ mc prods)
		= inside ("In the concstructor "++nm++": ") $
			do	validateComment' mc
				let usedNms = mapMaybe fst prods
				assert (unique usedNms) $ "a field name should be unique in each constructor. You used "++intercalate ", " (dubbles usedNms)++" at least twice"
				validateTypes tlt frees $ map snd prods


crossValidate	:: TypeLookupTable -> [Name] -> [ADTSum] -> Check
crossValidate tlt frees sums
		= do	let getProds (ADTSum cons _ _ prods)	= zip (mapMaybe unpackMaybeTuple prods) $ repeat cons
			let prodss 	= concatMap getProds sums
			let names	= map (fst . fst) prodss	:: [Name]
			mapM_ (crossValidateField tlt frees prodss) $ nub names


crossValidateField	:: TypeLookupTable -> [Name] -> [((Name,Type),Name)] -> Name -> Check
crossValidateField tlt frees prodss field
		= do	let foundTypes		= map (first snd) $ filter ((==) field . fst . fst) prodss	:: [(Type, Name)]
			let resolveT (t,c)	= validateType tlt frees t |> (\rt -> (rt, c))
			foundRTypesCons		<- mapM resolveT foundTypes	-- found types + constructor in which they were used
			let (pivot:foundRTypes)	=  map fst foundRTypesCons
			-- All types should be the same, thus all types should equal the first
			let showOne (rt,c)	= show rt ++ " in '"++c++"'"
			assert (allSame foundRTypes) $
				"Multiple types were found for the field '"++field++"': "++intercalate ", " (map showOne foundRTypesCons)
