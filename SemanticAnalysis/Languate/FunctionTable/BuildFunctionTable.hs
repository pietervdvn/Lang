module Languate.FunctionTable.BuildFunctionTable (buildFunctionTables) where

import StdDef

import Exceptions
import Languate.CheckUtils


import Languate.AST
import Languate.TAST
import Languate.FQN
import Languate.Package
import Languate.TypeTable
import Languate.PrecedenceTable

import Languate.FunctionTable
import Languate.FunctionTable.FunctionsIn

import Graphs.DirectedGraph
import Graphs.ExportCalculator hiding (importGraph)

import Data.Map hiding (filter)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Maybe (fromMaybe)
import Data.List

import Control.Arrow

-- Builds the function tables without implementation table, which will still get filled by buildFunctionTable itself. The raw refers to the untyped clauses
buildFunctionTables	:: Package -> TypeTable -> Exc (FunctionTables, Map FQN (Map Signature [Clause]))
buildFunctionTables p tt
	= do	-- TODO check for doubly defined functions
		functiontables'	<- p & modules & toList & mapM (buildFunctionTable' p tt)
					:: Exc [(FQN, (FunctionTable, Map Signature [Clause]))]
		let rawDict	= functiontables' ||>> snd & M.fromList
		let functiontables	= functiontables' ||>> fst & M.fromList
		let impGr	= importGraph p
		let getPubl fqn	= fromMaybe S.empty (M.lookup fqn functiontables
					|> public)
		let restrictions fqn (impFrom, funcSign)
				= isRestricted p fqn impFrom funcSign
		let exported	= calculateExports impGr (invert impGr) getPubl restrictions
		let ftsWithPub	= mergeTables exported functiontables
		let knowns	= calculateImports impGr getPubl exported
					|> S.toList ||>> fst
		let ftsWithKnown
			= mapWithKey (addKnown knowns) ftsWithPub
		return (FunctionTables ftsWithKnown, rawDict)

-- sets the known field
addKnown	:: Map FQN [Signature] -> FQN -> FunctionTable -> FunctionTable
addKnown knowns fqn fts
	= let	-- picks the correct known table out of the dict, which has been importcalced
		signs	= findWithDefault (error $ "no known table for "++show fqn) fqn knowns
		-- brings all in the correct form
		dict	= signs |> (signName &&& id) & merge ||>> nub & M.fromList in
		fts {known = dict}

isRestricted	:: Package -> FQN -> FQN -> Signature -> Bool
isRestricted pack currentModule importedFrom sign
	= let 	mod	= findWithDefault (error $ "Where is module "++show currentModule++"?") currentModule $ modules pack
		restr	= exports mod in
		isAllowed restr (signName sign)


mergeTables	:: Map FQN (Set (Signature, FQN)) -> Map FQN FunctionTable ->
			Map FQN FunctionTable
mergeTables exported
	= mapWithKey $ \fqn ft ->
		let	exp	= findWithDefault S.empty fqn exported & S.toList |> fst & S.fromList in
			ft {public = S.union exp $ public ft}


buildFunctionTable'		:: Package -> TypeTable -> (FQN, Module) -> Exc (FQN, (FunctionTable, Map Signature [Clause]))
buildFunctionTable' p tt (fqn, m)
	=  do	ft 	<- buildFunctionTable p tt fqn m
		return (fqn, ft)

buildFunctionTable		:: Package -> TypeTable -> FQN -> Module -> Exc (FunctionTable, Map Signature [Clause])
buildFunctionTable p tt fqn m	= inFile fqn $ do
	tlt	<- (typeLookups tt & M.lookup fqn) ? ("No tlt for "++show fqn)	:: Exc TypeLookupTable
	signs	<- mapM (functionIn' fqn tlt) (statements' m) |> concat		:: Exc [((Signature, Visible), (Signature, [Clause]), Coor)]
	let rawTable	= signs |> snd3 & M.fromList
	let defined	= signs |> (fst3 &&& thd3) |> first fst & M.fromList
	let public	= signs |> fst3 & filter ((==) Public . snd) |> fst & S.fromList
	return (FunctionTable defined public (todos "Known tables. Should be filled") , rawTable)
