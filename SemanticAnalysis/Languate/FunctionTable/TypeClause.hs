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
	ft		<- (to & functionTables & unpackFTS & M.lookup fqn) ?
				"No function table found?"
	let tt		= to & typeTable
	let expr	= expr2prefExpr (precedenceTable to) e
	let frees	= (rtypeUn >>= freesInRT) ++ (reqs >>= freesInReq)
	-- bind function with context. Reminder: t0 is the subtype, t1 the supertype
	let bnd	reqs' t0 t1
			= bind tt (M.fromList $ reqs ++ reqs') t0 t1
	-- returns true if binding is successfull
	let bnd' reqs' t0 t1	= isRight $ bnd reqs' t0 t1

	let curries	= rtypeUn |> curriedTypes
	let argTypes'	= curries |> tail' & L.filter (not . L.null)
	let returns	= curries |> last & nub
	if L.null argTypes' then do
		-- the expression is a constant
		texpr	<- expr2texpr p to fqn frees M.empty expr
		errIf (L.null texpr) $ "The expression "++show expr++" is not typeable"
		-- there are no arguments. The expression we return, should be every needed return type
		-- we bind every type
		let texpr' = texpr |> (id &&& typeOf)
				||>> substituteAway frees
				||>> returnsValid bnd' returns & L.filter snd
				|> fst
		haltIf (L.null texpr') $ "The expression "++show expr++" does not meet the required types."++indent ("\nNeeded types:\t"++ (returns |> show & commas)++"\nAvailable types:\t"++(texpr |> typeOf |> fst ||>> show |> commas & unlines))
		return $ TClause [] $ head texpr'
	else do
	let argTypes	= head argTypes'
	--(tpats, scopes)	<- patterns	& zip argTypes |> uncurry (typePattern ft)
	--				& sequence |> unzip
	--localScope	<- mergeDicts scopes ||>> (\t -> ([t],[]))
	--tes		<- expr2texpr p to fqn frees localScope $ expr2prefExpr (precedenceTable to) e
	--assert (not $ L.null tes) "Could not type expression"
	-- TODO select appropriate Texp
	return $ TClause [] $ TLocalCall "Abc" ([anyType],[])

{-
Each wanted type should be satisfied by at least one available type.
Thus: each wanted type should have at least one available type for which the available type binds into the wanted type
-}
returnsValid	:: (RTypeReqs -> RType -> RType -> Bool) -> [RType] ->
			([RType], RTypeReqs) -> Bool
returnsValid bnd wantedTypes (availableTypes, reqs)
	= wantedTypes
		|> (\super -> availableTypes |> (\av -> bnd reqs av super)) |> or & and
