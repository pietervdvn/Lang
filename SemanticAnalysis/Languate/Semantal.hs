module Languate.Semantal where

{--
Stub to export most common functions
--}

import StdDef
import Exceptions
import Languate.CheckUtils

import Languate.TableOverview
import Languate.AST
import Languate.TAST
import Languate.FQN
import Languate.Package
import Languate.Precedence.PrecedenceTable
import Languate.Precedence.Expr2PrefExpr
import Languate.FunctionTable.TypeExpr

import Data.Map as M


-- Types the given expression within context: the FQN in which it should be evaluated, the free type variables and the local scope (by pattern matches)
typeExpr	:: Package -> TableOverview -> FQN -> [Name] -> Map Name (RTypeUnion, RTypeReqs) ->
			Expression -> Exc [TExpression]
typeExpr package tablesOverv location frees localScope expr
	= do	let prefExpr	= expr & expr2prefExpr (precedenceTable tablesOverv)
		expr2texpr package tablesOverv location frees localScope prefExpr
