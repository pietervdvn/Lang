module Languate.FunctionTable.BuildImplementationTables where

{--
This module builds the implementations and sticks them into the FT
--}

import StdDef
import Normalizable
import Exceptions
import Languate.CheckUtils

import Languate.TAST
import Languate.AST
import Languate.FQN
import Languate.Package
import Languate.Typetable.TypeLookupTable
import Languate.FunctionTable.Def
import Languate.Typing.TypeExpression

import Data.Map as M
import Data.Set as S
import Data.List as L

-- Builds the implementations for the given module (FQN), which already has the visible functions in it's function table (given)
buildImplementations	:: Package -> Map FQN TypeLookupTable -> FQN -> (FunctionTable, Map Signature [Clause]) -> Exc FunctionTable
buildImplementations pack tlts fqn (ft, untClauses)
	= inside ("While building the implementations of the functions defined in "++show fqn) $
	  do	mod	<- M.lookup fqn (modules pack) ? ("No module found")
		tlt	<- M.lookup fqn tlts ? ("No tlt found")
		let funcs = statements mod & L.filter isFunctionStm |> (\(FunctionStm f) -> f)	:: [Function]
		imps 	<- funcs |+> buildImplementation tlt fqn |> concat |> M.fromList
		imps'	<- dictMapM  typeClauses' untClauses
		return (ft {implementations = M.unions [imps, imps', implementations ft]})



buildImplementation	:: TypeLookupTable -> FQN -> Function -> Exc [(Signature, [TClause])]
buildImplementation tlt fqn function
	= do	-- the function might have multiple declared types (e.g. (+) : Nat' -> Nat' -> Nat' and (+) : Nat -> Nat -> Nat)
		-- we get all possible signatures here
		rSigns	<- signs function |+> resolveSignature tlt fqn
		let cls	= clauses function
		rSigns |+> typeClauses cls

typeClauses	:: [Clause] -> Signature -> Exc (Signature, [TClause])
typeClauses clauses sign
		= do	tclauses	<- typeClauses' sign clauses
			return (sign, tclauses)


typeClauses'	:: Signature -> [Clause] -> Exc [TClause]
typeClauses' sign clauses
		= clauses |+> typeClause sign

typeClause	:: Signature -> Clause -> Exc TClause
typeClause sign (Clause pats expr)
		= do	--tpats	<- typePatterns pats
			texpr	<- typeExpr $ normalize expr
			return $ TClause [] texpr
