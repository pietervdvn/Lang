module Languate.Checks.Checks0 (validateWorld0) where

{--

This module implements all kind of usefull checks!

These checks only depend on the TYPE LOOKUP TABLE as context

--}

import StdDef
import Regex
import Exceptions

import Languate.TypeTable
import Languate.AST
import Languate.TAST
import Languate.FQN
import Languate.World
import Languate.Checks.CheckUtils

import Data.Char
import Data.Maybe
import qualified Data.Map as M
import Data.Map (Map, findWithDefault)
import Data.List



validateWorld0	:: Map FQN TypeLookupTable -> World -> Check
validateWorld0 tlts w
		=  mapM_ (validateModule' tlts w) $ M.keys $ modules w

validateModule' :: Map FQN TypeLookupTable -> World -> FQN -> Check
validateModule' tlts w fqn
		= do	let tlt	= findWithDefault M.empty fqn tlts
			mod <- (M.lookup fqn $ modules w) ? (show fqn ++ " not found in modules world")
			inFile fqn $ validateModule tlt fqn mod

validateModule	:: TypeLookupTable -> FQN -> Module -> Check
validateModule tlt fqn@(FQN _ _ (ModName name)) mod
		= do	assert (moduleName mod == name) $ "Invalid module name. Expected "++show name++" but got "++show (moduleName mod)
			mapM_ (validateStm' tlt) $ statements' mod


validateStm'	:: TypeLookupTable -> (Statement, Coor) -> Check
validateStm' tlt (stm,coor)
		= onLine coor $ validateStm tlt stm


validateStm	:: TypeLookupTable -> Statement -> Check
validateStm tlt (FunctionStm f)	= validateFunction tlt f
validateStm tlt (ADTDefStm adt)	= validateADTDef tlt adt
validateStm _ (Comments comms)
		= do	mapM_ validateComment comms
validateStm tlt (SubDefStm subdef)	= validateSubDef tlt subdef
validateStm tlts stm	= warn $ "Install checks for "++show stm


validateADTDef tlt (ADTDef nm frees reqs docStr sums)
		= inside ("In the definition of ADT-type "++show nm) $
		   do	validateComment docStr
			validateReqs tlt frees reqs
			validateReqsFreeOrder reqs frees
			mapM_ (validateADTSum tlt frees) sums
			let cons = map (\(ADTSum nm _ _ _) -> nm) sums
			-- TODO crosvalidate names vs types (data A = A b:X | B b:Y): b should have the same type
			assert (unique cons) $ "A constructor name should be unique. You used "++ intercalate ", " (dubbles cons) ++ " at least twice"


validateADTSum tlt frees (ADTSum nm _ mc prods)
		= inside ("In the definition of "++nm++": ") $
			do	validateComment $ fromMaybe "" mc
				let usedNms = mapMaybe fst prods
				assert (unique usedNms) $ "a field name should be unique in each constructor. You used "++intercalate ", " (dubbles usedNms)++" at least twice"
				validateTypes tlt frees $ map snd prods

validateFunction	:: TypeLookupTable -> Function -> Check
validateFunction tlt (Function docStr _ signs laws clauses)
		= inside ("In the function declaration of "++intercalate ", " nms) $
		   do	mapM_ validateLaw laws
			mapM_ (validateSign tlt) signs
			validateComment docStr
			where nms	= nub $ map (\(n,_,_) -> n) signs


validateSubDef tlt (SubDef nm _ frees superTps trex)	-- RAR! T-Rexes are allowed, velociraptors aren't
		= inside ("In the subtype declaration of "++nm) $
			do	validateTypes tlt frees superTps
				validateReqs tlt frees trex
				validateReqsFreeOrder trex frees


validateLaw law	= pass	-- TODO validate laws

validateSign	:: TypeLookupTable -> (Name, Type, [TypeRequirement]) -> Check
validateSign tlt (name, t, tr)
		=  do	validateType tlt (freesIn t) t
			validateReqs tlt (freesIn t) tr





validateComment comment
		= let	caseIns	str	= concatMap (\c -> '[':toLower c:toUpper c : "]") str
			choice keyWords	=  (\s -> "(" ++ s ++ ")") $ intercalate "|" $ map caseIns keyWords
			rgx	= regex $ choice ["todo","fixme","fix me","fix-me"] ++"!\n*"
			todos	= mapMaybe (longestMatch rgx) $ lines comment in
			mapM_ warn todos



validateReqs tlt frees	= mapM_ (validateType tlt frees . snd)


validateTypes tlt frees	= mapM (validateType tlt frees)
validateType tlt frees t
		= do	rt 	<- catch (resolveType tlt t) (recover t)
			let foundFrees	= freesIn t
			mapM (\a -> assert (a `elem` frees) $ "The free type variable "++show a++" wat not declared") foundFrees
			return rt


recover	t e	= do	err $ "Failed lookup of the type "++show t++".\nContinuing with checks anyway."
			err e
			return $ RFree "FAILED lookup"
{-
Validates that, when a free is used in a typerequirement, this free is declared and the declaration is on the left of it's usage.
E.g:
data A a (b:X a) is valid, but
data A (a:X b) b	 is not
-}
validateReqsFreeOrder	:: [TypeRequirement] -> [Name] -> Check
validateReqsFreeOrder reqs order
			= mapM_ (_vAllrfo reqs) $ tail' $ inits order

_vAllrfo		:: [TypeRequirement] -> [Name] -> Check
_vAllrfo reqs defined@(free:_)
			= do	let tps	= map snd $ filter ((==) free . fst) reqs
				let stackMsg	= "In the type requirements for "++free++": "++intercalate "," (map show tps)
				inside stackMsg $ mapM_ (_vrfo defined) tps


-- validate that frees in type is declared
_vrfo		:: [Name] -> Type -> Check
_vrfo known t	=  do	let used	= freesIn t
			let faulty	= filter (`notElem` known) used
			let msg a	= "The free type variable "++a++" is not known in the type application of "++show t++".\n Make sure "++a++" is declared before (on the left side) of its usage."
			mapM_ (err . msg) faulty
