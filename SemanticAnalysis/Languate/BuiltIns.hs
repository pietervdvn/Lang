module Languate.BuiltIns where

{--
This module gives fixed signatures of builtin functions.
Also see TAST, where FQNs of builtin types are given.
--}

import StdDef
import Languate.TAST
import Languate.AST
import Languate.FQN


-- gives fqn of flip, which is needed to desugar sections: (:1) -> flip (:) 1
flipSign	:: (FQN, Name)
flipSign	= (toFQN' "pietervdvn:Data:ControlFlow","flip"  )

-- some types with more meaning (and a broken liskov substitution)

boolType'	= Normal ["Data","Bool"] "Bool"

maybeType'	:: [Type] -> Type
maybeType'	= Applied (Normal ["Collection","Maybe"] "Maybe")

-- Special function for constructing ADTS
construct	:: Int -> Int -> (Type, [TypeRequirement]) -> Clause
construct nrOfArgs i typInfo
		=  let argNames	= [0..nrOfArgs-1] |> show |> ("arg"++) in
			Clause (argNames |> Assign) $
			Seq ([BuiltIn "construct" typInfo, Nat i] ++ argNames |> Call)

-- Construction primitves which can not be encoded as code (e.g. construct, destruct) and are language features live in the nameless package by pietervdvn

constructTCall	:: (RType, RTypeReqs) -> Int -> TExpression
constructTCall (rt, reqs) i
	= let	frees	= freesInRT rt ++ (reqs >>= freesInReq)
		argTps	= defaultFreeNames & filter (not . (`elem` frees)) & take i
		typ	= uncurriedTypes $ natType:(argTps |> RFree)++[rt] in
		TCall $ Signature (toFQN' "pietervdvn::BuiltIns") "construct" [typ] reqs

destruct	:: Int -> (Type, [TypeRequirement]) -> Expression
destruct i (t,treqs)
		= todo
