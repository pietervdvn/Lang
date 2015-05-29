module Languate.FunctionTable.TypeClause where

import StdDef
import Exceptions
import Languate.CheckUtils
import HumanUtils

import Languate.AST
import Languate.TAST
import Languate.FQN
import Languate.Package
import Languate.TypeTable
import Languate.TypeTable.Bind.Bind
import Languate.TypeTable.Bind.Substitute
import Languate.TableOverview

import Languate.FunctionTable
import Languate.FunctionTable.TypeExpr
import Languate.FunctionTable.TypePattern

import Languate.Precedence.Expr2PrefExpr

import Data.Map as M
import Data.List as L

import Control.Arrow

typeClause	:: Package -> TableOverview -> FQN -> RTypeUnion -> RTypeReqs -> Clause -> Exc TClause
typeClause p to fqn rtypeUn reqs clause@(Clause patterns e)= do

	{- set up -}
	ft		<- (to & functionTables & unpackFTS & M.lookup fqn) ?
				"No function table found?"
	let tt		= to & typeTable
	let expr	= expr2prefExpr (precedenceTable to) e
	let frees	= (rtypeUn >>= freesInRT) ++ (reqs >>= freesInReq)
	let isSubT	= isSubtype tt (reqs & M.fromList)
	-- bind function with context. Reminder: t0 is the subtype, t1 the supertype
	let bnd	reqs'	= bind tt (M.fromList $ reqs ++ reqs')
	-- returns true if binding is successfull
	let bnd' reqs' t0	= isRight . bnd reqs' t0

	-- calculate, for each type, how many arguments it needs
	let curryNumbers	= rtypeUn |> curryNumber tt
	let currErr rt curryN	= st True rt ++"\t takes "++ plural curryN "argument"
	assert (allSame curryNumbers) $ "The types in the union take a different number of arguments: "++indent ('\n' : zip rtypeUn curryNumbers |> uncurry currErr & unlines )

	-- the number of arguments the function takes
	let numberOfArg	= head curryNumbers
	let numberOfPat	= length patterns
	errIf (numberOfPat > numberOfArg) $ "Too much patterns: "++plural numberOfPat "pattern match"++" "++isAre numberOfPat++
		" given, but at most "++number numberOfArg++" "++isAre numberOfArg++" expected."++indent "\n(This is not Haskell, you don't have to type the function name :p )"

	-- actual argument types of the function
	let curries	= rtypeUn |> curriedTypes
	let argTypes'	= curries |> init' & L.filter (not . L.null)
	let returns	= curries |> drop numberOfPat & L.filter (not . L.null) |> uncurriedTypes & nub
	(texprs, pats)	<- if L.null argTypes' then do
				-- the expression is a constant
				texprs'	<- expr2texpr p to fqn frees M.empty expr
				return (texprs', [])
			    else do
				-- TODO argument types are slightly more complicated than just selecting a random one
				let argTypes	= head argTypes'
				(tpats, scopes)	<- inside "While typing the patterns" $
							patterns & zip argTypes |> uncurry (typePattern isSubT ft)
							& sequence |> unzip
				localScope	<- mergeDicts scopes ||>> (\t -> ([t],[]))
				texprs'		<- inside "While typing the expression" $
							expr2texpr p to fqn frees localScope $ expr2prefExpr (precedenceTable to) e
				return (texprs', tpats)


	errIf (L.null texprs) $ "The expression "++show expr++" is not typeable"
	-- The expression we return, should be every needed return type.
	-- we bind every type
	let texprs' = texprs |> (id &&& typeOf)
				||>> substituteAway frees
				||>> returnsValid bnd' returns & L.filter snd
				|> fst
	haltIf (L.null texprs') $ "The expression "++show expr++" does not meet the required types."++
			indent ("\nNeeded types:\t"++ (returns |> show & commas)++
				"\nAvailable types:\t"++(texprs' |> typeOf |> fst ||>> show |> commas & unlines))
	-- TODO select best one
	let texpr	= head texprs'
	return $ TClause pats texpr

{-
Each wanted type should be satisfied by at least one available type.
Thus: each wanted type should have at least one available type for which the available type binds into the wanted type
-}
returnsValid	:: (RTypeReqs -> RType -> RType -> Bool) -> [RType] ->
			([RType], RTypeReqs) -> Bool
returnsValid bnd wantedTypes (availableTypes, reqs)
	= wantedTypes
		|> (\super -> availableTypes |> (\av -> bnd reqs av super)) |> or & and
