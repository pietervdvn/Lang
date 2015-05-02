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
import Data.List

import Control.Arrow

buildFunctionTables	:: Package -> TypeTable -> Exc FunctionTables
buildFunctionTables p tt
	= do	let bft (fqn, m)= do	ft 	<- buildFunctionTable p tt fqn m
					return (fqn, ft)
		-- TODO check for doubly defined functions
		functiontables	<- p & modules & toList & mapM bft |> fromList
		let impGr	= importGraph p
		let getPubl fqn	= fromMaybe S.empty (M.lookup fqn functiontables
					|> public)
		let restrictions fqn (impFrom, funcSign)
				= isRestricted p fqn impFrom funcSign
		let exported	= calculateExports impGr (invert impGr) getPubl restrictions
		let ftsWithPub	= mergeTables exported functiontables
		let known	= calculateImports impGr getPubl exported
					|> S.toList ||>> fst
		let ftsWithKnown
			= mapWithKey (addKnown known) ftsWithPub
		return $ FunctionTables ftsWithKnown

addKnown	:: Map FQN [Signature] -> FQN -> FunctionTable -> FunctionTable
addKnown known fqn fts
	= let	signs	= findWithDefault (error $ "no known table for "++show fqn) fqn known
		dict	= signs |> (signName &&& id) & merge ||>> nub & M.fromList
		msg	= "The implementation of the function (in the function table) is not yet added. The FTs should still be completed" in
		fts {known = dict ||>> (id &&& const (error msg))}

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


buildFunctionTable		:: Package -> TypeTable -> FQN -> Module -> Exc FunctionTable
buildFunctionTable p tt fqn m = inFile fqn $
	do	tlt	<- (typeLookups tt & M.lookup fqn) ? ("No tlt for "++show fqn)
		signs	<- mapM (functionIn' fqn tlt) (statements' m) |> concat
		let defined	= signs |> fst
		let public	= signs & filter ((==) Public . snd) |> fst
		return $ FunctionTable (S.fromList defined) (S.fromList public) (error $ "Non overwritten known table in function table for "++show fqn++", this is a bug")

functionIn'	:: FQN -> TypeLookupTable -> (Statement, Coor) ->
			Exc [(Signature, Visible)]
functionIn' fqn tlt (stm, coor) = onLine coor $
	do	let signs	= functionIn stm
		mapM (\((nm, t, reqs),v) -> do	rt	<- mapM (resolveType tlt) t
						rreqs	<- mapM (resolveTypeIn tlt) reqs
						return (Signature fqn nm rt $ merge rreqs, v)  )  signs



{-
Searches for function declarations within the statements
-}
functionIn	:: Statement -> [((Name, [Type], [TypeRequirement]), Visible)]
functionIn (FunctionStm f)
	= zip (signs f) (repeat $ visibility f)
functionIn (ClassDefStm cd)
	= let	signs	= decls cd |> third3 (classReqs cd ++) in
		zip signs $ repeat Public
functionIn (ADTDefStm (ADTDef nm frees reqs sums))
	= let	typ	= Applied (Normal [] nm) (frees |> Free) in
		sums |> functionsInADTSum (typ, reqs) & concat
functionIn _
	= []

functionsInADTSum 	:: (Type, [TypeRequirement]) -> ADTSum ->
				[((Name, [Type], [TypeRequirement]), Visible)]
functionsInADTSum (t,treqs) (ADTSum consName vis args)
	= let	argTypes	= args |> snd
		constructor	= ((consName, [Curry $ argTypes++[t]], treqs), vis) in
		[constructor]	-- TODO add field getters, setters and modders
