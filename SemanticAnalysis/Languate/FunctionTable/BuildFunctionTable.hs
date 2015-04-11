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

import Graphs.DirectedGraph
import Graphs.ExportCalculator hiding (importGraph)

import Data.Map hiding (filter)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Maybe (fromMaybe)

import Control.Arrow

buildFunctionTables	:: Package -> TypeTable -> Exc FunctionTables
buildFunctionTables p tt
	= do	let bft (fqn, m)= do	ft 	<- buildFunctionTable p tt fqn m
					return (fqn, ft)
		functiontables	<- p & modules & toList & mapM bft |> fromList
		let impGr	= importGraph p
		let getPubl fqn	= fromMaybe S.empty $ (M.lookup fqn functiontables
					|> public)
		let restrictions fqn (impFrom, funcSign)
				= isRestricted p fqn impFrom funcSign
		let exported	= calculateExports impGr (invert impGr) getPubl restrictions
		return $ FunctionTables $ mergeTables exported functiontables


isRestricted	:: Package -> FQN -> FQN -> Signature -> Bool
isRestricted pack currentModule importedFrom sign
	= let 	mod	= findWithDefault (error $ "Where is module "++show currentModule++"?") currentModule $ modules pack
		restr	= exports mod in
		isAllowed restr (signName sign)


mergeTables	:: Map FQN (Set (Signature, FQN)) -> Map FQN FunctionTable -> 
			Map FQN FunctionTable
mergeTables exported fts
	= mapWithKey (\fqn ft ->
		let	exp	= findWithDefault S.empty fqn exported & S.toList |> fst & S.fromList in
			ft {public = S.union exp $ public ft}
		) fts

buildFunctionTable		:: Package -> TypeTable -> FQN -> Module -> Exc FunctionTable
buildFunctionTable p tt fqn m = inFile fqn $
	do	tlt	<- (typeLookups tt & M.lookup fqn) ? ("No tlt for "++show fqn)
		signs	<- mapM (functionIn' tlt) (statements' m) |> concat
		let defined	= signs |> fst
		let public	= signs & filter ((==) Public . snd) |> fst
		return $ FunctionTable (S.fromList defined) (S.fromList public)

functionIn'	:: TypeLookupTable -> (Statement, Coor) ->
			Exc [(Signature, Visible)]
functionIn' tlt (stm, coor) = onLine coor $
	do	let signs	= functionIn stm
		mapM (\((nm, t, reqs),v) -> do	rt	<- mapM (resolveType tlt) t
						rreqs	<- mapM (resolveTypeIn tlt) reqs
						return (Signature nm rt rreqs, v)  )  signs



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
