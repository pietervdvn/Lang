module Languate.FunctionTable.BuildImplementationTables where

{--
This module builds the implementations and sticks them into the FT
--}

import StdDef
import Exceptions
import Languate.CheckUtils

import Languate.TAST
import Languate.AST
import Languate.FQN
import Languate.Package
import Languate.Typetable.TypeLookupTable
import Languate.FunctionTable.Def

import Data.Map as M
import Data.Set as S
import Data.List as L

-- Builds the implementations for the given module (FQN), which already has the visible functions in it's function table (given)
buildImplementations	:: Package -> Map FQN TypeLookupTable -> FQN -> FunctionTable -> Exc FunctionTable
buildImplementations pack tlts fqn ft
	= inside ("While building the implementations of the functions defined in "++show fqn) $
	  do	mod	<- M.lookup fqn (modules pack) ? ("No module found")
		tlt	<- M.lookup fqn tlts ? ("No tlt found")
		let funcs = statements mod & L.filter isFunctionStm |> (\(FunctionStm f) -> f)	:: [Function]
		imps 	<- funcs |+> buildImplementation tlt fqn |> concat
		return (ft {implementations = M.fromList imps})



buildImplementation	:: TypeLookupTable -> FQN -> Function -> Exc [(Signature, [TClause])]
buildImplementation tlt fqn function
	= do	-- the function might have multiple declared types (e.g. (+) : Nat' -> Nat' -> Nat' and (+) : Nat -> Nat -> Nat)
		-- we get all possible signatures here
		rSigns	<- signs function |+> resolveSignature tlt fqn
		return (zip rSigns $ repeat [])