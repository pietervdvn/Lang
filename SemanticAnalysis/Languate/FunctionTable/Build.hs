module Languate.FunctionTable.Build where


import StdDef
import HumanUtils
import Exceptions
import Languate.CheckUtils

import Languate.Package
import Languate.FQN
import Languate.AST
import Languate.TAST
import Languate.Typetable

import Languate.FunctionTable.Def
import Languate.FunctionTable.Utils
import Languate.FunctionTable.ModuleTraverser

import Data.Set as S
import Data.Map as M
import Data.List as L

import Control.Arrow

{-
How are the function tables built?
-- TODO: all below this line
-> We start by looking what functions are defined, with the according types
	- we check for double defined functions
-> We propagate these definitions, so that each module knows what functions are visible
-> We build the implementations of these defined functions
	- and typecheck those
Mo

-}


buildFunctionTables	:: Package
				-> Map FQN TypeLookupTable
				-> Map FQN Typetable
				-> Map FQN Module
				-> Exc (Map FQN FunctionTable)
buildFunctionTables p tlts tts mods
 	= dictMapM (buildFunctionTable tlts) mods

buildFunctionTable	:: Map FQN TypeLookupTable -> FQN -> Module -> Exc FunctionTable
buildFunctionTable tlts fqn mod
	= inside ("While building the function table for "++show fqn) $
	  do	tlt	<- M.lookup fqn tlts ? ("No tlt for "++show fqn)
		signs'	<- mod & statements' |+> onFirst (definedFuncSign mod tlt fqn)
				||>> unpackFirst |> concat	:: Exc [((Signature, Visible), Coor)]
		let signs	= signs' |> fst			:: [(Signature, Visible)]
		let dubble	= signs |> fst & dubbles	:: [Signature]
		let dubble'	= signs' |> first fst |> second fst & nub	-- throw away visibility and columns, then remove dubbles
					& L.filter ((`elem` dubble) . fst)
					|> (\(sign, line) -> show sign ++" "++pars ("line "++show line))
		assert (L.null dubble) ("Some functions are declared multiple times:"++indent (dubble' >>= ("\n"++)))
		return $ FunctionTable $ M.fromList signs
