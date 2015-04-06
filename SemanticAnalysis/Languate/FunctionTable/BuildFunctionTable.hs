module Languate.FunctionTable.BuildFunctionTable where

import StdDef

import Exceptions
import Languate.CheckUtils


import Languate.AST
import Languate.TAST
import Languate.FQN
import Languate.Package
import Languate.TypeTable

import Languate.FunctionTable

import Data.Map
import Data.Map as M

import Control.Arrow

buildFunctionTables	:: Package -> TypeTable -> Exc FunctionTables
buildFunctionTables p tt
	= p & modules & toList
		& mapM (\(fqn, m) -> do	ft 	<- buildFunctionTable p tt fqn m
					return (fqn, ft))
		|> fromList


buildFunctionTable		:: Package -> TypeTable -> FQN -> Module -> Exc FunctionTable
buildFunctionTable p tt fqn m = inFile fqn $
	do	tlt	<- (typeLookups tt & M.lookup fqn) ? ("No tlt for "++show fqn)
		signs	<- mapM (functionIn' tlt) $ statements' m
		return $ FunctionTable $ fromList $ concat signs

functionIn'	:: TypeLookupTable -> (Statement, Coor) -> Exc [(Name, [RType])]
functionIn' tlt (stm, coor) = onLine coor $
	do	let signs	= functionIn stm
		mapM (resolveTypesIn tlt) signs


{-
Searches for function declarations within the statements
-}
functionIn	:: Statement -> [(Name, [Type])]
functionIn (FunctionStm f)
	= signs f |> (fst3 &&& snd3)
functionIn (ClassDefStm cd)
	= decls cd |> (fst3 &&& snd3)
functionIn _
	= []
