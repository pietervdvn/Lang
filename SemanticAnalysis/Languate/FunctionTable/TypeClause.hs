module Languate.FunctionTable.TypeClause where

import StdDef
import Exceptions
import Languate.CheckUtils

import Languate.AST
import Languate.TAST
import Languate.FQN
import Languate.Package
import Languate.TypeTable
import Languate.TableOverview

import Languate.FunctionTable
import Languate.FunctionTable.TypeExpr
import Languate.FunctionTable.TypePattern

import Languate.Precedence.Expr2PrefExpr

import Data.Map as M
import Data.List as L

import Control.Arrow

import Debug.Trace

typeClause	:: Package -> TableOverview -> FQN -> RTypeUnion -> RTypeReqs -> Clause -> Exc TClause
typeClause p to fqn rtypeUn reqs (Clause patterns e)= do
	ft		<- (to & functionTables & unpackFTS & M.lookup fqn) ?
				"No function table found?"
	let frees	= rtypeUn >>= freesInRT
	let argTypes	= rtypeUn |> curriedTypes |> tail & L.filter (not . L.null) & head
	(tpats, scopes)	<- patterns	& zip argTypes |> uncurry (typePattern ft)
					& sequence |> unzip
	--localScope	<- mergeDicts scopes ||>> (\t -> ([t],[]))
	--tes		<- expr2texpr p to fqn frees localScope $ expr2prefExpr (precedenceTable to) e
	--assert (not $ L.null tes) "Could not type expression"
	-- TODO select appropriate Texp
	return $ TClause [] $ TLocalCall "Abc" ([anyType],[])
