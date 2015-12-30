module Languate.Typing.TypePattern where

import StdDef
import HumanUtils
import Exceptions
import Languate.CheckUtils
import Languate.FunctionTable.Def
import Languate.TypeConstraint
import Languate.FQN

import Languate.Typetable

import Languate.AST
import Languate.TAST

import Languate.Typing.TypeExpression

import Data.Set as S
import Data.Map as M
import Data.List as L
import Data.Char

import Data.Maybe
import Control.Arrow
import Control.Monad

{-
tables: info about the context
reqs: requirements on the types
localscope: scope created by left patterns (e.g. 'a b $(==a+b)')
Argument types. Each argument might be an intersection of mutliple things; this is saved within the
Actual patterns
-> (typed patterns, local scope (including previous scope), curried variables (start with _a))

-}
typePatterns	:: (FunctionTable, Typetable) -> FullRTypeReq -> LocalScope -> [RType] -> [Pattern] -> Exc ([TPattern], LocalScope, [Name])
typePatterns tables reqs ls args pats
		= inside ("In the typing of the pattern "++ (pats |> show & unwords & pars)++ " with expected types "++(args |> show & commas)) $
			do	pats'		<- demultidontcare (length args) pats
				let curried	= take ((length args) - (length pats')) defaultFreeNames |> ("_"++)
				let pats''	= pats' ++ (curried |> Assign)
				let typePat (acc, scope) (pat, arg)
						= do	(pat', scope')	<- typePattern tables reqs scope pat arg
							return (acc++[pat'], scope')	:: Exc ([TPattern], LocalScope)
				(tPats, newScope)	<- zip args pats'' & foldM typePat ([], ls)
				return (tPats, newScope, curried)

{- old scope is embedded within the new, returned scope
	Patterns may never match, then a big fat N
	The expected type might be an intersection. This can should expressed in the type requirements
-}
typePattern	:: (FunctionTable, Typetable) -> FullRTypeReq -> LocalScope -> RType -> Pattern -> Exc (TPattern, LocalScope)
typePattern _ _ ls _ DontCare
	= return (TDontCare, ls)
typePattern _ _ ls inTyp (Assign n)
	= do	ls'	<- safeUnion ls $ M.singleton n inTyp
		return (TAssign n, ls')
typePattern tables reqs ls inTyp (Multi pats)
	= do	let typePat (acc, scope) pat	= do	(pat', scope')	<- typePattern tables reqs scope inTyp pat
							return (acc++[pat'], scope')
		(tpats, scope)	<- foldM typePat ([], ls) pats
		return (TMulti tpats, scope)
typePattern tables@(ft, tt) rqs ls inTyp (Deconstruct n' pats)
	-- we search all function with given name and type (A -> b) or (A -> Maybe b) (where b might be a tuple)
	= do	let n	= if isUpper $ head n' then "from"++n' else n'
		_funcs	<- visibleFuncs ft & M.lookup n ? ("No function with the name "++n++" found")
		let funcs	= S.toList _funcs	:: [(Signature, [FQN])]
		funcs'	<- funcs |> (fst &&& id)	-- [(Signature, (Signature, [FQN]))]
				|+> onSecond (onFirst (deconstructorArgs tt inTyp))
				|> L.filter (isJust . fst . snd)
				:: Exc [(Signature, (Maybe [RType], [FQN]))]
		haltIf (L.null funcs') $ "No suitable deconstructor function found for "++show n++indent ("\n"++
			(funcs |> fst |> show |> ("Tried "++) |> (++" but it didn't have a suitable type") & unlines))
		assert (length funcs' == 1) $ "Multiple deconstructor functions found for "++show n++indent ("\n"++
			funcs' |> (\(sign, (_, impFrom)) -> show sign ++  "(imported from "++ (impFrom |> show & commas) ++")") & unlines
			)
		let (sign, argTps)	= head funcs' & second fst |> fromJust	-- we assume only one function is found, thus the head
		(tpats, scope, curries)	<- typePatterns tables rqs ls argTps pats
		assert (L.null curries) $ ("The destructor "++show n++" is not applied to enough arguments.")
		return (TDeconstruct sign tpats, scope)
typePattern tables rqs ls rts (Eval expr)
	= do	texprs	<- typeExpr tables rqs ls expr
		if L.null texprs then do
			err $ "Could not type the 'same-as' pattern "++show expr
			return (TFail, ls)
		else do
		{- typeExpr returns multiple, possible expressions. We only keep the one of which the type matches this type
		   The expression should return a single value, which can be compared to the incoming type, in other words,
			f	: X -> Y
			$e	= Y

			e	:: T

		  Thus: T `subtypeOf` X
		  	e.g.
		  	f	: Nat -> ...
		  	$Zero	= ...
		  or	T `superTypeOf` X
		  	e.g.
		  	areSame	: Int -> Nat -> True
		  	i $i	= True
		  	_ _	= False

		  So you could say some types shuold be in common. Even this is not true, as multiple implementations might be shared over multiple signatures
		  e.g.
		  (+)	: Nat' -> Nat' -> Nat'
		  (+)	: Nat -> Nat -> Nat
		  Zero Zero	= Zero
		  (Succ i) ...	= Succ ...

		  The first clause will never match for the type signature "Nat' -> Nat' -> Nat'".
		  In this case, the clause is dead (for this typing - it might be usefull for other types).
		  We return a 'TFail'; the totality checker will pick it up and warn for clauses that are never used
		-}
		let tps	= texprs |> (id &&& typeOf)
		-- TODO:
		-- we also check that the type that gets in, has Eq as super type (as we want to compare it)
		warn $ show texprs	-- TODO remove err
		return (TDontCare, ls)
typePattern _ _ ls rt p
	= do	warn $ "TODO: unsupported pattern "++show p++ " with expected type "++show rt
		-- TODO
		return (TDontCare, ls) --}



-- Returns wether or not the type of this expression can be used. Expected is an intersection of the expected value to end in the pattern
isValidEvalType		:: (FunctionTable, Typetable) -> CType -> [RType] -> Exc Bool
isValidEvalType (ft, tt) (ret, reqs) expected
	= do	let reqs'	= asConstraints reqs
		-- first we check wether the return type is a subtype of the expected type, e.g. (Zero `elem` Nat) by binding
		-- bind will assume the left is a subtype. We have a match if

		return False -- TODO pickup



-- it is a decons type if we take a single argument, and return a normal value, a tuple, or a maybe of a normal value/tuple
deconstructorArgs	:: Typetable -> RType -> Signature -> Exc (Maybe [RType])
deconstructorArgs tt originType sign
	= do	(argTypes, rtType, reqs)	<- unpackArgs $ signType sign	:: Exc ([RType], RType, RTypeReq)
		let reqs'	= asConstraints reqs
		-- the originType will be passed into the function (as argType)
		-- The originType might be *bigger* then the argTypes. If it's a non matching value, the pattern match will simply fail and move on
		-- It might even happen that the in type is totality incompatible. This can happen with implementation sharing. We simply return a fail then
		-- TODO think thoroughly about it!

		-- at this point, we only have to take a look at the output type(s)
		-- Are we dealing with a Maybe? If yes, then all return types should bind on "Maybe a"
		constrs	<- bind (RApplied maybeType $ RFree "_decons") rtType
				& allNeededConstraintsFor tt reqs'
				||>> S.toList	:: Exc (Maybe [TypeConstraint])
					-- Returns the typeunion of what is in the maybe if this binding succeeded
		let maybeArgs'	= unpackMaybeArgFromConstraints constrs	:: Maybe RType
		let tupleArgs	= fromMaybe rtType maybeArgs'
		-- TODO FIXME use actual binding! Use actual intersections on the type!
		let args	= tupleArgs & tupledTypes
		return $ Just args


unpackMaybeArgFromConstraints	:: Maybe [TypeConstraint] -> Maybe RType
unpackMaybeArgFromConstraints constraints
	= do	constraints'	<- constraints
		-- we assume the only left constraint is [SubTypeConstr (RFree "_decons") _actual tuple value_ ]
		-- if any other constraint pops up, we can't bind and it's not a maybe type after all!
		case constraints' of
			([SubTypeConstr (RFree "_decons") tuplVal])	-> return tuplVal
			_	-> Nothing


{- we get a signature of a function, which is applied on a single argument, namely inTyp.
	We thus bind this argument type onto the
	-}
unpackArgs	:: CType -> Exc ([RType], RType, RTypeReq)
unpackArgs (tp, reqs)
	= do	-- TODO FIXME tp should be bound against (RCurry _a _b). If this succeeds, we now the first argument and have already a single type
		let curried	= curriedTypes tp
		let argTypes	= init curried
		let rtType	= last curried
		return (argTypes, rtType, reqs)

-------------------- UTILS ---------------------

safeUnion	:: LocalScope -> LocalScope -> Exc LocalScope
safeUnion a b
	= do	let dub	= dubbles (M.keys a ++ M.keys b)
		let msg	var
			= "Multiple declarations of the variable "++show var++
				"\nIf you want both arguments to be the same, use " ++ ('$':var)++" for the second."
		assert (L.null dub) $ (dub |> msg |> indent & unlines)
		return $ M.union a b

safeUnions	:: [LocalScope] -> Exc LocalScope
safeUnions []	= return M.empty
safeUnions (l:ls)
		= safeUnions ls >>= safeUnion l


-- expands the '*' into _zero_ or more DontCares. Errs on multiple stars
demultidontcare	:: Int -> [Pattern] -> Exc [Pattern]
demultidontcare i pats
	= do	let (before, after')	= break (==MultiDontCare) pats
		if L.null after' then return pats else do
		let after = tail' after'
		assert (MultiDontCare `notElem` after) $ ("The pattern "++pars (show pats)++" contains mutliple '*', which is not allowed")
		let after'	= after & L.filter (/=MultiDontCare)
		let needed	= i - (length before + length after)
		return (before ++ replicate needed DontCare ++ after)
