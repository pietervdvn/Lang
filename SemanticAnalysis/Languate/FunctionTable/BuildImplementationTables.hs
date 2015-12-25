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
import Languate.Typing.TypePattern

import Data.Map as M
import Data.Set as S
import Data.List as L

import Data.Tuple
import Control.Arrow

-- Builds the implementations for the given module (FQN), which already has the visible functions in it's function table (given)
buildImplementations	:: Package -> Map FQN TypeLookupTable -> FQN -> (FunctionTable, Map Signature [Clause]) -> Exc FunctionTable
buildImplementations pack tlts fqn (ft, untClauses)
	= inside ("While building the implementations of the functions defined in "++show fqn) $
	  do	mod	<- M.lookup fqn (modules pack) ? ("No module found")
		tlt	<- M.lookup fqn tlts ? ("No tlt found")
		let funcs = statements mod & L.filter isFunctionStm |> (\(FunctionStm f) -> f)	:: [Function]
		imps 	<- funcs |+> buildImplementation tlt fqn |> concat |> M.fromList
		imps'	<- dictMapM  typeClauses untClauses
		return (ft {implementations = M.unions [imps, imps', implementations ft]})



buildImplementation	:: TypeLookupTable -> FQN -> Function -> Exc [(Signature, [TClause])]
buildImplementation tlt fqn function
	= do	-- the function might have multiple declared types (e.g. (+) : Nat' -> Nat' -> Nat' and (+) : Nat -> Nat -> Nat)
		-- we get all possible signatures here
		rSigns	<- signs function |+> resolveSignature tlt fqn	:: Exc [Signature]
		let cls	= clauses function
		(zip rSigns (repeat cls) |> (fst &&& id)) 	-- [(Signature, (Signature, [Clause]))]
			|+> onSecond (uncurry typeClauses)

typeClauses	:: Signature -> [Clause] -> Exc [TClause]
typeClauses sign clauses
		= inside ("While typing the function "++show sign) $
		  do	-- the signature contains a typeUNION, what means it should meet **all** of the given types.
			-- TODO for now, we assume all these types have the same number of args
			let (tps, reqs)	= signTypes sign
			let curried	= tps |> curriedTypes	-- [ [arg0 -> arg1 -> rt ], [arg0' -> arg1' -> rt'], ... ]
			assert (curried |> length & allSame) $ "Contradictory number of argument in the different number of types:" ++ indent  ("\n" ++ tps |> show & unlines)
			let argTypes	= transpose curried	-- [ [arg0, arg0', arg0''], [arg1, arg1',...], [rt, rt', ...] ]

			clauses |+> typeClause reqs argTypes

typeClause	:: RTypeReq -> [[RType]] -> Clause -> Exc TClause
typeClause reqs args' (Clause pats expr)
		= do	let args	= init args'
			let rt		= last args'
			(tpats, lscope, curries)	<- typePatterns reqs args pats
			texpr	<- typeExpr $ normalize expr
			return $ TClause tpats texpr
