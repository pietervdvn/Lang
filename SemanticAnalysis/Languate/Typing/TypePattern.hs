module Languate.Typing.TypePattern where

import StdDef
import HumanUtils
import Exceptions
import Languate.CheckUtils

import Languate.AST
import Languate.TAST

import Data.Map as M
import Data.List as L


{-
Argument types; each argument might be an intersection of mutliple things
Patterns
(typed patterns, local scope, curried variables (start with _a))

-}
typePatterns	:: RTypeReq -> [[RType]] -> [Pattern] -> Exc ([TPattern], LocalScope, [Name])
typePatterns reqs args pats
		= inside ("In the typing of the pattern "++ (pats |> show & unwords & pars)++ " with expected types "++(args |> show & commas)) $
			do	pats'		<- demultidontcare (length args) pats
				(tPats, scopes)	<- zip args pats' |+> uncurry typePattern |> unzip
				scopes'		<- safeUnions scopes
				return (tPats, scopes', [])

typePattern	:: [RType] -> Pattern -> Exc (TPattern, LocalScope)
typePattern _ DontCare
		= return (TDontCare, M.empty)
typePattern rts (Assign n)
		= return (TAssign n, M.singleton n rts)
typePattern rts (Deconstruct n pats)
		= return (TDontCare, M.empty)	-- TODO
typePattern rt p
		= return (TDontCare, M.empty)	-- TODO halt $ "TODO: pattern "++show p++ " with expected type "++show rt







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
	= do	let (before, after)	= break (==MultiDontCare) pats
		assert (MultiDontCare `notElem` after) $ ("The pattern "++pars (show pats)++" contains mutliple '*', which is not allowed")
		let after'	= after & L.filter (/=MultiDontCare)
		let needed	= i - (length before + length after)
		return (before ++ replicate needed DontCare ++ after)
