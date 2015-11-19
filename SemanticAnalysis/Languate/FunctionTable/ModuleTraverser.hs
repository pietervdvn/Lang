module Languate.FunctionTable.ModuleTraverser where

{--
This module traverser the module, to fetch function statements
--}

import StdDef
import Exceptions
import Languate.CheckUtils

import Languate.FQN
import Languate.AST
import Languate.TAST
import Languate.Typetable hiding (frees)
import Languate.FunctionTable.ConstructADTFunctions
import Languate.FunctionTable.Def

import Data.List

import Control.Arrow


-- fetches the function signatures that are defined within the statement
definedFuncSign	:: Module -> TypeLookupTable -> FQN -> Statement -> Exc [(Signature, (Visible, Generated, Abstract))]
definedFuncSign m tlt fqn (FunctionStm function)
	= do	defs	<- signs function |+> resolveSignature tlt fqn
		visibs	<- defs |+> getVisibility m (visibility function)
		let visibsGen	= visibs |> (\visib -> (visib, False, Nothing))
		return (zip defs visibsGen)

definedFuncSign m tlt fqn (ADTDefStm adtDef)
	= adtDefinedFunctions tlt fqn adtDef |||>>> (\sign -> (sign, True, Nothing))

definedFuncSign m tlt fqn (SubDefStm (SubDef nm vis frees tps reqs))
	= do	defType		<- resolveType tlt (Normal [] nm)
		rtps		<- tps |+> resolveType tlt
		rreqs		<- resolveReqs tlt reqs
		let defType'	= applyTypeArgsFree defType frees
		-- type of the function to convert
		-- we pick a new free name, which should be of all the given types
		let free	= (defaultFreeNames \\ frees) & head	:: Name
		let singleType	= length rtps == 1
		let startT	= if singleType then head rtps else RFree free
		let extraReq	= if singleType then [] else [(free, rtps)]
		let funcT	= RCurry startT defType'
		let funcReqs	= rreqs ++ extraReq
		return [(Signature fqn ("as"++nm) ([funcT], funcReqs), (Private, True, Nothing))]

definedFuncSign m tlt fqn (ClassDefStm cd)
	= do	defType	<- resolveType tlt (Normal [] $ name cd)
		let defFree	= take (length $ frees cd) $ defaultFreeNames
		let defType'	= applyTypeArgsFree defType defFree
		signs	<- decls cd |+> resolveSignature' tlt fqn
		return (zip signs $ repeat (Public, False, Just defType'))

definedFuncSign _ _ _ _
	= return []
