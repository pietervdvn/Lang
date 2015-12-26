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
import Languate.Typetable

import Data.Map as M
import Data.Set as S
import Data.List as L

import Data.Tuple
import Control.Arrow

-- Builds the implementations for the given module (FQN), which already has the visible functions in it's function table (given)
buildImplementations	:: Package -> Map FQN TypeLookupTable -> Map FQN Typetable -> FQN -> (FunctionTable, Map Signature [Clause]) -> Exc FunctionTable
buildImplementations pack tlts tts fqn (ft, untClauses)
	= inside ("While building the implementations of the functions defined in "++show fqn) $
	  do	mod	<- M.lookup fqn (modules pack) ? ("No module found")
		tlt	<- M.lookup fqn tlts ? ("No tlt found")
		tt	<- M.lookup fqn tts ? "No tt found"
		let funcs = statements mod & L.filter isFunctionStm |> (\(FunctionStm f) -> f)	:: [Function]
		imps 	<- funcs |+> buildImplementation tlt fqn (ft, tt) |> concat |> M.fromList
		imps'	<- dictMapM  (typeClauses (ft, tt) ) untClauses
		return (ft {implementations = M.unions [imps, imps', implementations ft]})



buildImplementation	:: TypeLookupTable -> FQN -> (FunctionTable, Typetable) -> Function -> Exc [(Signature, [TClause])]
buildImplementation tlt fqn tables function
	= do	-- the function might have multiple declared types (e.g. (+) : Nat' -> Nat' -> Nat' and (+) : Nat -> Nat -> Nat)
		-- we get all possible signatures here
		rSigns	<- signs function |+> resolveSignature tlt fqn	:: Exc [Signature]
		let cls	= clauses function
		(zip rSigns (repeat cls) |> (fst &&& id)) 	-- [(Signature, (Signature, [Clause]))]
			|+> onSecond (uncurry $ typeClauses tables)

typeClauses	:: (FunctionTable, Typetable) -> Signature -> [Clause] -> Exc [TClause]
typeClauses tables sign clauses
		= inside ("While typing the function "++show sign) $
		  do	-- the signature contains a typeUNION, what means it should meet **all** of the given types.
			-- TODO for now, we assume all these types have the same number of args
			(argTypes, rtTypes, reqs)	<- unpackArgs $ signTypes sign
			clauses |+> typeClause tables reqs argTypes rtTypes

typeClause	:: (FunctionTable, Typetable) -> RTypeReq -> [[RType]] -> [RType] -> Clause -> Exc TClause
typeClause tables reqs args rt (Clause pats expr)
		= do	(tpats, lscope, curries)	<- typePatterns tables reqs args pats
			texpr	<- typeExpr $ normalize expr
			return $ TClause tpats texpr
