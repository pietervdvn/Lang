module Languate.FunctionTable.TypeClause where

import StdDef
import Exceptions
import Languate.CheckUtils

import Languate.AST
import Languate.TAST
import Languate.FQN
import Languate.Package
import Languate.TypeTable
import Languate.TableOverview

import Languate.FunctionTable.Expr2Texpr
import Languate.Precedence.Expr2PrefExpr

import Data.Map as M
import Data.List as L

import Control.Arrow

typeClause	:: Package -> TableOverview -> FQN -> RTypeUnion -> RTypeReqs -> Clause -> Exc TClause
typeClause p to fqn rtypeUn reqs (Clause patterns e)= do
	let frees	= rtypeUn >>= freesInRT
	let argTypes	= rtypeUn |> curriedTypes |> tail & L.filter (not . L.null) & head
	(tpats, scopes)	<- patterns	& zip argTypes |> uncurry typePattern & sequence
					|> unzip
	localScope	<- mergeDicts scopes
	tes		<- expr2texpr p to fqn frees $ expr2prefExpr (precedenceTable to) e
	-- TODO select appropriate Texp
	return $ TClause tpats $ head tes


-- Types the given pattern (or gives an exception)
typePattern	:: RType -> Pattern -> Exc (TPattern, Map Name RType)
typePattern tp (Assign nm)
		= return (TAssign nm, M.singleton nm tp)
typePattern tp pat@(Multi pats)
		= do	(tpat, dicts) 	<- pats	|> typePattern tp & sequence
						|> unzip |> first TMulti
			inside ("While typing the pattern "++show pat) $ do
			dict	<- mergeDicts dicts
			return (tpat, dict)



mergeDicts	:: [Map Name RType] -> Exc (Map Name RType)
mergeDicts dicts = do
	let allkeys	= dicts >>= M.keys
	assert (unique allkeys) $ "Multiple declarations of the variables "++(dubbles allkeys |> show & unwords)
	return $ M.unions dicts
