module Languate.Checks.Checks0 where

{--

This module implements all kind of usefull checks!

These checks only depend on the TYPE LOOKUP TABLE as context

--}

import StdDef
import Exceptions
import Languate.TypeTable
import Languate.AST
import Languate.TAST
import Languate.FQN
import Languate.World
import Data.Char
import Data.Maybe
import qualified Data.Map as M
import Data.Map (Map, findWithDefault)
import Data.List
import Regex

type Check	= Exceptions' String ()

validateWorld	:: Map FQN TypeLookupTable -> World -> Check
validateWorld tlts w
		=  mapM_ (validateModule' tlts w) $ M.keys $ modules w

validateModule' :: Map FQN TypeLookupTable -> World -> FQN -> Check
validateModule' tlts w fqn
		= do	let tlt	= findWithDefault M.empty fqn tlts
			mod <- (M.lookup fqn $ modules w) ? (show fqn ++ " not found in modules world")
			stack' (("In the file "++show fqn++":")++) $ validateModule tlt fqn mod

validateModule	:: TypeLookupTable -> FQN -> Module -> Check
validateModule tlt fqn@(FQN _ _ (ModName name)) mod
		= do	assert (moduleName mod == name) $ "Invalid module name. Expected "++show name++" but got "++show (moduleName mod)
			mapM_ (validateStm' tlt) $ statements' mod


validateStm'	:: TypeLookupTable -> (Statement, Coor) -> Check
validateStm' tlt (stm,(l,c))
		=  stack' (("\nOn line "++show l ++":\n")++) $ validateStm tlt stm


validateStm	:: TypeLookupTable -> Statement -> Check
validateStm tlt (FunctionStm f)	= validateFunction tlt f
validateStm tlt (ADTDefStm adt)	= validateADTDef tlt adt
validateStm _ (Comments comms)
		= do	mapM_ validateComment comms
validateStm tlts stm	= pass -- warn $ "Install checks for "++show stm


validateADTDef tlt (ADTDef nm frees reqs docStr sums)
		= do	validateComment docStr
			validateReqs tlt reqs
			mapM_ (validateADTSum tlt) sums
			let cons = map (\(ADTSum nm _ _ _) -> nm) sums
			-- TODO crosvalidate names vs types (data A = A b:X | B b:Y): b should have the same type
			assert (unique cons) $ "A constructor name should be unique. You used "++ intercalate ", " (dubbles cons) ++ " at least twice"


validateADTSum tlt (ADTSum nm _ mc prods)
		= stack' (("In the constructor "++nm++": ")++) $
			do	validateComment $ fromMaybe "" mc
				let usedNms = mapMaybe fst prods
				assert (unique usedNms) $ "a field name should be unique in each constructor. You used "++intercalate ", " (dubbles usedNms)++" at least twice"
				mapM_ (validateType tlt . snd) prods

validateFunction	:: TypeLookupTable -> Function -> Check
validateFunction tlt (Function docStr _ signs laws clauses)
		= do	mapM_ validateLaw laws
			mapM_ (validateSign tlt) signs
			validateComment docStr


validateLaw law	= pass

validateSign tlt (name, t, tr)
		=  do	validateType tlt t
			validateReqs tlt tr

validateComment comment
		= let	caseIns	str	= concatMap (\c -> '[':toLower c:toUpper c : "]") str
			choice keyWords	=  (\s -> "(" ++ s ++ ")") $ intercalate "|" $ map caseIns keyWords
			rgx	= regex $ choice ["todo","fixme","fix me","fix-me"] ++"!\n*"
			todos	= mapMaybe (longestMatch rgx) $ lines comment in
			mapM_ warn todos

validateType	:: TypeLookupTable -> Type -> Exceptions' String RType
validateType tlt (Normal path nm)
		= stack' (("The type "++show (intercalate "." $ path++[nm]) ++ " ") ++) $ do
			let notFoundMsg	= err "can not be resolved"
			case safeResolveType tlt (path, nm) of
				Nothing		-> do	err "can not be resolved"
							return $ RCurry []
				Just [fqn]	-> return $ RNormal fqn nm
				Just []		-> do	err "can not be resolved (bug)"
							return $ RCurry []
				Just fqns	-> do	err $ " is ambigous. It can refer to "++intercalate ", " (map (\fqn -> show fqn ++ "." ++ nm) fqns)
							return $ RCurry []

validateType tlt (Applied t tps)
		= do	t' 	<- validateType tlt t
			tps' 	<- mapM (validateType tlt) tps
			return $ RApplied t' tps'
validateType tlt (Curry tps)		= mapM (validateType tlt) tps >>= return . RCurry
validateType tlt (TupleType tps)	= mapM (validateType tlt) tps >>= return . RTuple
validateType tlt (Free a)	= return $ RFree a
validateType _ Infer	= do	err "Unresolved Infer"
				return $ RCurry []

validateReqs tlt	= mapM_ (validateType tlt . snd)

unique ls	= length ls == (length $ nub ls)
