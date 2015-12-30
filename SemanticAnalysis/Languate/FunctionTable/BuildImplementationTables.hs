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

import Languate.PrecedenceTable

import Data.Map as M
import Data.Set as S
import Data.List as L

import Data.Tuple
import Control.Arrow

-- Builds the implementations for the given module (FQN), which already has the visible functions in it's function table (given)
buildImplementations	:: Package -> PrecedenceTable -> Map FQN TypeLookupTable -> Map FQN Typetable -> FQN -> (FunctionTable, Map Signature [Clause]) -> Exc FunctionTable
buildImplementations pack precT tlts tts fqn (ft, untClauses)
	= inside ("While building the implementations of the functions defined in "++show fqn) $
	  do	mod	<- M.lookup fqn (modules pack) ? ("No module found")
		tlt	<- M.lookup fqn tlts ? ("No tlt found")
		tt	<- M.lookup fqn tts ? "No tt found"
		imps	<- dictMapM  (typeClauses precT (ft, tt) ) untClauses
		return (ft {implementations = M.unions [imps, implementations ft]})


typeClauses	:: PrecedenceTable -> (FunctionTable, Typetable) -> Signature -> [Clause] -> Exc [TClause]
typeClauses precT tables sign clauses
		= inside ("While typing the function "++show sign) $
		  do	-- the signature contains a typeUNION, what means it should meet **all** of the given types.
			-- TODO for now, we assume all these types have the same number of args
			(argTypes, rtTypes, reqs)	<- unpackArgs $ signType sign
			clauses |> rewriteClauseExprs precT |+> typeClause tables reqs argTypes rtTypes

typeClause	:: (FunctionTable, Typetable) -> RTypeReq -> [RType] -> RType -> Clause -> Exc TClause
typeClause tables reqs args rt (Clause pats expr)
		= do	let usedFrees	= (rt:args) >>= freesInRT
			let usedFrees'	= usedFrees & L.filter (`L.notElem` (reqs |> fst))
			let fullReqs	= Reqs $ (reqs ++ (zip usedFrees $ repeat []))
			(tpats, lscope, curries)	<- typePatterns tables fullReqs M.empty args pats
			texprs	<- typeExpr tables fullReqs lscope expr
			if length texprs == 0 then do
				err $ "No valid typing found for "++show expr
				return $ TClause tpats (TLocalCall (RFree "a") "NO TYPING FOUND!")
			else do
				assert (length texprs < 2) $ "Multiple implementations are possible"++
					indent ("\n"++(texprs |> (\e -> show e ++": "++show (typeOf e)) & unlines))
				return $ TClause tpats $ head texprs

rewriteClauseExprs	:: PrecedenceTable -> Clause -> Clause
rewriteClauseExprs precT (Clause pats expr)
	= let	expr'	= rewriteExpression precT expr
		pats'	= pats |> rewritePatternExprs (rewriteExpression precT)
		in Clause pats' expr'

-- removes useless comments and rewrites the expression into it's prefix form
rewriteExpression	:: PrecedenceTable -> Expression -> Expression
rewriteExpression precT expr
	= expr & removeExpNl & expr2prefExpr precT & normalize
