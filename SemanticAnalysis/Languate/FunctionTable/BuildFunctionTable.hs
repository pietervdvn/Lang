module Languate.FunctionTable.BuildFunctionTable where

import StdDef

import Exceptions
import Languate.CheckUtils


import Languate.AST
import Languate.TAST
import Languate.FQN
import Languate.Package
import Languate.TypeTable
import Languate.Precedence.PrecedenceTable

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

buildFunctionTables	:: Package -> TypeTable -> Exc (FunctionTables, Map Module (Map Signature [Clause]))
buildFunctionTables p tt
	= do	let bft (fqn, m)= do	ft 	<- buildFunctionTable p tt fqn m
					return (fqn, fst ft)
		-- TODO check for doubly defined functions
		functiontables	<- p & modules & toList & mapM bft |> fromList
		let impGr	= importGraph p
		let getPubl fqn	= fromMaybe S.empty (M.lookup fqn functiontables
					|> public)
					-- :: (FunctionTable, Map Signature [Clause])
		let restrictions fqn (impFrom, funcSign)
				= isRestricted p fqn impFrom funcSign
		let exported	= calculateExports impGr (invert impGr) getPubl restrictions
		let ftsWithPub	= mergeTables exported functiontables
		let known	= calculateImports impGr getPubl exported
					|> S.toList ||>> fst
		let ftsWithKnown
			= mapWithKey (addKnown known) ftsWithPub
		let rawDict	= todo	-- TODO
		return (FunctionTables ftsWithKnown, rawDict)

addKnown	:: Map FQN [Signature] -> FQN -> FunctionTable -> FunctionTable
addKnown known fqn fts
	= let	signs	= findWithDefault (error $ "no known table for "++show fqn) fqn known
		-- TODO
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


buildFunctionTable		:: Package -> TypeTable -> FQN -> Module -> Exc (FunctionTable, Map Signature [Clause])
buildFunctionTable p tt fqn m	= inFile fqn $ do
	tlt	<- (typeLookups tt & M.lookup fqn) ? ("No tlt for "++show fqn)	:: Exc TypeLookupTable
	signs	<- mapM (functionIn' fqn tlt) (statements' m) |> concat ||>> fst
	let defined	= signs |> fst
	let public	= signs & filter ((==) Public . snd) |> fst
	return (FunctionTable (S.fromList defined) (S.fromList public) (todos "Known table") (todos "Implementation table"), todos "Raw table")	-- TODO
