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

import Data.Set as S
import Data.Map as M
import Data.List as L
import Data.Char

import Data.Maybe
import Control.Arrow


{-
Argument types; each argument might be an intersection of mutliple things
Patterns
(typed patterns, local scope, curried variables (start with _a))

-}
typePatterns	:: (FunctionTable, Typetable) -> RTypeReq -> [[RType]] -> [Pattern] -> Exc ([TPattern], LocalScope, [Name])
typePatterns tables reqs args pats
		= inside ("In the typing of the pattern "++ (pats |> show & unwords & pars)++ " with expected types "++(args |> show & commas)) $
			do	pats'		<- demultidontcare (length args) pats
				(tPats, scopes)	<- zip args pats' |+> uncurry (typePattern tables reqs) |> unzip
				scopes'		<- safeUnions scopes
				return (tPats, scopes', [])

typePattern	:: (FunctionTable, Typetable) -> RTypeReq -> [RType] -> Pattern -> Exc (TPattern, LocalScope)
typePattern _ _ _ DontCare
	= return (TDontCare, M.empty)
typePattern _ _ rts (Assign n)
	= return (TAssign n, M.singleton n rts)
typePattern tables reqs rts (Multi pats)
	= do	(tpats, scopes)	<- pats |+> typePattern tables reqs rts |> unzip
		scopes'		<- safeUnions scopes
		return (TMulti tpats, scopes')
typePattern tables@(ft, tt) rqs rts (Deconstruct n' pats)
	-- we search all function with given name and type (A -> b) or (A -> Maybe b) (where b might be a tuple)
	= do	let n	= if isUpper $ head n' then "from"++n' else n'
		_funcs	<- visibleFuncs ft & M.lookup n ? ("No function with the name "++n++" found")
		let funcs	= S.toList _funcs
		funcs'	<- funcs |> (fst &&& id)|+> onSecond (onFirst (deconstructorArgs tt rts)) |> L.filter (isJust . fst . snd)
				:: Exc [(Signature, (Maybe [[RType]], [FQN]))]
		assert (not $ L.null funcs') $ "No suitable deconstructor function found for "++show n++indent ("\n"++
			(funcs |> fst |> show |> ("Tried "++) |> (++" but it didn't have a suitable type") & unlines))
		assert (length funcs' == 1) $ "Multiple deconstructor functions found for "++show n++indent ("\n"++
			funcs' |> (\(sign, (_, impFrom)) -> show sign ++  "(imported from "++ (impFrom |> show & commas) ++")") & unlines
			)
		let (sign, argTps)	= head funcs' & second fst |> fromJust
		(tpats, scope, curries)	<- typePatterns tables rqs argTps pats
		assert (L.null curries) $ ("The destructor "++show n++" is not applied to enough arguments.")
		return (TDeconstruct sign tpats, scope)	-- TODO

typePattern _ _ rt p
	= {- halt $ "TODO: pattern "++show p++ " with expected type "++show rt -} return (TDontCare, M.empty) --}



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
		constrs	<- rtTypes |> SubTypeConstr (RApplied maybeType $ RFree "_decons") |> (:[])
				|+> allNeededConstraints tt reqs'
				|||>>> S.toList	:: Exc [Maybe [TypeConstraint]]
					-- Returns the typeunion of what is in the maybe if this binding succeeded
		-- TODO this looks broken
		let maybeArgs'	= unpackMaybeArgFromConstraints constrs	:: Maybe [RType]
		let tupleArgs = fromMaybe rtTypes maybeArgs'

		-- TODO FIXME use actual binding! Use actual unions!
		let args	= head tupleArgs & tupledTypes
		warn $ show args
		return $ Just [args]


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
