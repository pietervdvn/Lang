module Languate.FunctionTable.FunctionsIn where

{--
This module implements functions in, which gives headers and implementations
--}
import StdDef
import Exceptions
import Languate.CheckUtils

import Languate.AST
import Languate.TAST
import Languate.FQN
import Languate.Package
import Languate.TypeTable

import Languate.FunctionTable
import Languate.Precedence.PrecedenceTable

data SimpleFunc	= SimpleFunc {	funcName	:: Name,
				funcTypes	:: [Type],
				funcTypeReqs	:: [TypeRequirement],
				funcVisibility	:: Visible,
				funcClauses	:: [Clause] }


functionIn'	:: FQN -> TypeLookupTable -> (Statement, Coor) ->
			Exc [((Signature, Visible), (Signature, [Clause]))]
functionIn' fqn tlt (stm, coor) = onLine coor $
	do	let signs	= functionIn stm
		signs	|> (\func -> do	rt	<- mapM (resolveType tlt)	$ funcTypes func
					rreqs	<- mapM (resolveTypeIn tlt)	$ funcTypeReqs func
					let sign	= Signature fqn (funcName func) rt $ merge rreqs
					return ((sign, funcVisibility func), (sign, funcClauses func))  )
			& sequence

{-
Searches for function declarations within the statements
-}
functionIn	:: Statement -> [SimpleFunc]
functionIn (FunctionStm f)
	= makeFunc (signs f) (visibility f) (clauses f)
functionIn (ClassDefStm cd)
	= let	signs	= decls cd |> third3 (classReqs cd ++) in
		makeFunc signs Public []
functionIn (ADTDefStm (ADTDef nm frees reqs sums))
	= let	typ	= Applied (Normal [] nm) (frees |> Free) in
		sums >>= functionsInADTSum (typ, reqs)
functionIn _
	= []

functionsInADTSum 	:: (Type, [TypeRequirement]) -> ADTSum ->
				[SimpleFunc]
functionsInADTSum (t,treqs) (ADTSum consName vis args)
	= let	argTypes	= args |> snd
		constructor	= (consName, [Curry $ argTypes++[t]], treqs) in
		-- TODO add field getters, setters and modders
		-- TODO add implementation
		makeFunc [constructor] vis []

makeFunc	:: [(Name, [Type], [TypeRequirement])] -> Visible -> [Clause] -> [SimpleFunc]
makeFunc signs vis clauses
	= signs |> (\(nm, tps, reqs) -> SimpleFunc nm tps reqs vis clauses)
