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

-- UnResolved nat type
natTypeUR	= Normal ["Data","Nat"] "Nat"



-- Special function for constructing ADTS
construct	:: Int -> Int -> (Type, [TypeRequirement]) -> Clause
construct nrOfArgs i typeInfo
		=  let argNames	= [0..nrOfArgs-1] |> show |> ("arg"++) in
			Clause (argNames |> Assign) $
			Seq ([BuiltIn "construct" typeInfo, Nat i] ++ argNames |> Call)

-- special function for deconstructing ADTS. The passed typeInfo should be the type "value -> Maybe (a,b,c)"
deconstruct	:: Int -> (Type, [TypeRequirement]) -> Clause
deconstruct index typeInfo
	= Clause [Assign "value"] $ Seq [BuiltIn "deconstruct" typeInfo, Nat index, Call "value"]

is		:: Int -> (Type, [TypeRequirement]) -> Clause
is index typeInfo
	= Clause [Assign "value"] $ Seq [BuiltIn "is" typeInfo, Nat index, Call "value"]

-- Construction primitves which can not be encoded as code (e.g. construct, destruct) and are language features. These live in the nameless package by pietervdvn

builtInFQN	:: FQN
builtInFQN	= toFQN' "pietervdvn::BuiltIns"

constructTCall	:: (RType, RTypeReq) -> Int -> TExpression
constructTCall (rt, reqs) i
	= let	frees	= freesInRT rt ++ (reqs >>= freesInReq)
		argTps	= defaultFreeNames & filter (not . (`elem` frees)) & take i
		typ	= uncurriedTypes $ natType:(argTps |> RFree)++[rt] in
		TCall ([typ], reqs) $ Signature builtInFQN "construct" ([typ], reqs)

destructTCall	:: (RType, RTypeReq) -> TExpression
destructTCall (RCurry value result,reqs)
		= let typ	= RCurry natType $ RCurry value result in
 			TCall ([typ], reqs) $ Signature builtInFQN "deconstruct" ([typ], reqs)

isTCall		:: (RType, RTypeReq) -> TExpression
isTCall (value, reqs)
		= let typ	= RCurry natType $ RCurry value boolType in
			TCall ([typ], reqs) $ Signature builtInFQN "is" ([typ], reqs)
