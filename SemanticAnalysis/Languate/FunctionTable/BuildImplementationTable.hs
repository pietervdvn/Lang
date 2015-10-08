module Languate.FunctionTable.BuildImplementationTable where

import StdDef
import Exceptions
import Languate.CheckUtils

import Languate.Package
import Languate.AST
import Languate.TAST
import Languate.FQN
import Languate.TableOverview
import Languate.FunctionTable
import Languate.TypeTable
import Languate.PrecedenceTable

import Languate.FunctionTable.TypeClause

import Data.Map as M

import Debug.Trace

buildImplementationTables	:: Package -> TableOverview -> Map FQN (Map Signature [Clause]) -> Exc ImplementationTables
buildImplementationTables p to dict
	= dict & dictMapM (buildImpTable p to) |> ImplementationTables

buildImpTable	:: Package -> TableOverview -> FQN -> Map Signature [Clause] -> Exc ImplementationTable
buildImpTable p to fqn dict
	= dict	& dictMapM
		(\sign clauses -> try (\e -> err e >> return []) $ mapM (tClause p to fqn sign) clauses)
		|> ImplementationTable


tClause	:: Package -> TableOverview -> FQN -> Signature -> Clause -> Exc TClause
tClause p to fqn sign
	= inFile fqn .
	  inside ("While typing " ++signName sign) .
		typeClause p to fqn (signTypes sign) (signTypeReqs sign)
