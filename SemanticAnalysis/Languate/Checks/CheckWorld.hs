module Languate.Checks.CheckWorld (validateWorld0) where

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
import Languate.Checks.CheckFunction
import Languate.Checks.CheckComment
import Languate.Checks.CheckADT
import Languate.Checks.CheckSubDef




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
