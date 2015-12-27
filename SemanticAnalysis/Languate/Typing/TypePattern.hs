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
Argument types; each argument might be an intersection of mutliple things
Actual patterns
-> (typed patterns, local scope (including previous scope), curried variables (start with _a))

-}
typePatterns	:: (FunctionTable, Typetable) -> FullRTypeReq -> LocalScope -> [[RType]] -> [Pattern] -> Exc ([TPattern], LocalScope, [Name])
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

-- old scope is returned too
typePattern	:: (FunctionTable, Typetable) -> FullRTypeReq -> LocalScope -> [RType] -> Pattern -> Exc (TPattern, LocalScope)
typePattern _ _ ls _ DontCare
	= return (TDontCare, ls)
typePattern _ _ ls rts (Assign n)
	= do	ls'	<- safeUnion ls $ M.singleton n rts
		return (TAssign n, ls')
typePattern tables reqs ls rts (Multi pats)
	= do	let typePat (acc, scope) pat	= do	(pat', scope')	<- typePattern tables reqs scope rts pat
							return (acc++[pat'], scope')
		(tpats, scope)	<- foldM typePat ([], ls) pats
		return (TMulti tpats, scope)
typePattern tables@(ft, tt) rqs ls rts (Deconstruct n' pats)
	-- we search all function with given name and type (A -> b) or (A -> Maybe b) (where b might be a tuple)
	= do	let n	= if isUpper $ head n' then "from"++n' else n'
		_funcs	<- visibleFuncs ft & M.lookup n ? ("No function with the name "++n++" found")
		let funcs	= S.toList _funcs
		funcs'	<- funcs |> (fst &&& id)|+> onSecond (onFirst (deconstructorArgs tt rts)) |> L.filter (isJust . fst . snd)
				:: Exc [(Signature, (Maybe [[RType]], [FQN]))]
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
	= do	texpr	<- typeExpr tables rqs ls expr
		-- TODO
		return (TDontCare, ls)
typePattern _ _ ls rt p
	= do	warn $ "TODO: unsupported pattern "++show p++ " with expected type "++show rt
		-- TODO
		return (TDontCare, ls) --}



-- it is a decons type if we take a single argument, and return a normal value, a tuple, or a maybe of a normal value/tuple
deconstructorArgs	:: Typetable -> [RType] -> Signature -> Exc (Maybe [[RType]])
deconstructorArgs tt originTypes sign
	= do	(argTypess, rtTypes, reqs)	<- unpackArgs $ signTypes sign	:: Exc ([[RType]], [RType], RTypeReq)
		if (length argTypess) /= 1 then return Nothing else do
		let argTypes	= head argTypess
		let reqs'	= asConstraints reqs
		-- the originType will be passed into the function (as argType)
		-- The originType might be *bigger* then the argTypes. If it's a non matching value, the pattern match will simply fail and move on
		-- it however is problematic when not a single match can be made
		-- TODO think thoroughly about it!

		-- at this point, we only have to take a look at the output type(s)
		-- Are we dealing with a Maybe? If yes, then all return types should bind on "Maybe a"
		constrs	<- rtTypes |> bind (RApplied maybeType $ RFree "_decons") |> (:[])
				|+> allNeededConstraints tt reqs'
				|||>>> S.toList	:: Exc [Maybe [TypeConstraint]]
					-- Returns the typeunion of what is in the maybe if this binding succeeded
		let maybeArgs'	= unpackMaybeArgFromConstraints constrs	:: Maybe [RType]
		let tupleArgs	= fromMaybe rtTypes maybeArgs'
		-- TODO FIXME use actual binding! Use actual unions!
		let args	= head tupleArgs & tupledTypes
		return $ Just (args |> (:[]))


unpackMaybeArgFromConstraints	:: [Maybe [TypeConstraint]] -> Maybe [RType]
unpackMaybeArgFromConstraints constraints
 | not $ all isJust constraints	= Nothing
 | otherwise
	= do	-- we assume the only left constraint is [SubTypeConstr (RFree "_decons") _actual tuple value_ ]
		-- if any other constraint pops up, we can't bind and it's not a maybe type after all!
		constraints & catMaybes |+> validConstr
		where	validConstr [SubTypeConstr (RFree "_decons") tuplVal]	= Just tuplVal
			validConstr _	= Nothing



unpackArgs	:: CTypeUnion -> Exc ([[RType]], [RType], RTypeReq)
unpackArgs (tps, reqs)
	= do	let curried	= tps |> curriedTypes	-- [ [arg0 -> arg1 -> rt ], [arg0' -> arg1' -> rt'], ... ]
		-- TODO fix for cases as "Commutative a b" = "a -> a -> b"
		assert (curried |> length & allSame) $ "Contradictory number of argument in the different number of types:" ++ indent  ("\n" ++ tps |> show & unlines)
		let argsRetTypes	= transpose curried	-- [ [arg0, arg0', arg0''], [arg1, arg1',...], [rt, rt', ...] ]
		let argTypes	= init argsRetTypes
		let rtTypes	= last argsRetTypes
		return (argTypes, rtTypes, reqs)

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
