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
	- we check that functions have an appropriate, simple kind
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
 	= dictMapM (buildFunctionTable tlts tts) mods

buildFunctionTable	:: Map FQN TypeLookupTable -> Map FQN Typetable -> FQN -> Module -> Exc FunctionTable
buildFunctionTable tlts tts fqn mod
	= inside ("While building the function table for "++show fqn) $
	  do	tlt	<- M.lookup fqn tlts ? ("No tlt for "++show fqn)
	  	tt	<- M.lookup fqn tts ? ("No tt for "++show fqn)
		signs'	<- mod & statements' |+> onFirst (definedFuncSign mod tlt fqn)
				||>> unpackFirst |> concat	:: Exc [((Signature, (Visible, Generated)), Coor)]
		signs' |> first fst |+> checkSimpleKind tt
		let signs	= signs' |> fst			:: [(Signature, (Visible, Generated))]
		let dubble	= signs |> fst & dubbles	:: [Signature]
		let dubble'	= signs' |> first fst |> second fst & nub	-- throw away visibility and columns, then remove dubbles
					& L.filter ((`elem` dubble) . fst)
					|> (\(sign, line) -> show sign ++" "++pars ("line "++show line))
		assert (L.null dubble) ("Some functions are declared multiple times:"++indent (dubble' >>= ("\n"++)))
		return $ FunctionTable $ M.fromList signs




checkSimpleKind	:: Typetable ->  (Signature, Coor) -> Check
checkSimpleKind tt (sign, (line, _))
	= inside ("In the function declaration of "++show sign++", on line "++show line) $
	  do	let (rtps, reqs)	= signTypes sign
	  	let unusedFrees	= (rtps >>= freesInRT) L.\\ (reqs |> fst)
	  	let unusedReqs	= zip unusedFrees (repeat [anyType])
	  	reqKinds	<- kindOfReqs tt (reqs++unusedReqs)
	  	kinds		<-rtps |+> kindOf tt reqKinds
	  	let faulty	= zip rtps kinds & L.filter ((/=) Kind . snd)
	  				|> (\(tp, kind) -> "Expecting "++number (numberOfKindArgs kind)++" more type arguments to "++
	  					show tp++" :: "++show kind)
	  	assert (L.null faulty) "Function types should have a normal kind:"
