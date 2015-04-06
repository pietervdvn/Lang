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
import Graphs.ExportCalculator

import Data.Map hiding (filter)
import qualified Data.Map as M

import Control.Arrow

buildFunctionTables	:: Package -> TypeTable -> Exc FunctionTables
buildFunctionTables p tt
	= do	let bft (fqn, m)= do	ft 	<- buildFunctionTable p tt fqn m
					return (fqn, ft)
		functiontables	<- p & modules & toList & mapM bft |> fromList
		return functiontables


buildFunctionTable		:: Package -> TypeTable -> FQN -> Module -> Exc FunctionTable
buildFunctionTable p tt fqn m = inFile fqn $
	do	tlt	<- (typeLookups tt & M.lookup fqn) ? ("No tlt for "++show fqn)
		signs	<- mapM (functionIn' tlt) (statements' m) |> concat
		let defined	= signs |> fst
		let public	= signs & filter ((==) Public . snd) |> fst
		return $ FunctionTable (fromList defined) (fromList public)

functionIn'	:: TypeLookupTable -> (Statement, Coor) ->
			Exc [((Name, ([RType], [RTypeReq])), Visible)]
functionIn' tlt (stm, coor) = onLine coor $
	do	let signs	= functionIn stm
		mapM (\((nm, t, reqs),v) -> do	rt	<- mapM (resolveType tlt) t
						rreqs	<- mapM (resolveTypeIn tlt) reqs
						return ((nm, (rt, rreqs)), v)  )  signs



{-
Searches for function declarations within the statements
-}
functionIn	:: Statement -> [((Name, [Type], [TypeRequirement]), Visible)]
functionIn (FunctionStm f)
	= zip (signs f) (repeat $ visibility f)
functionIn (ClassDefStm cd)
	= let	signs	= decls cd |> (third3 $ (classReqs cd ++)) in
		zip signs $ repeat Public
functionIn _
	= []
