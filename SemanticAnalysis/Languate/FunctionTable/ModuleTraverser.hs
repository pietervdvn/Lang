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

data FunctionInfo	= FI { fiSign	:: Signature,
				fiVis	:: Visible,
				fiGen	:: Generated,
				fiAbs	:: Abstract,
				fiClauses	:: Maybe (Either [Clause] [TClause])
				}
	deriving (Show)
 -- (Signature, (Visible, Generated, Abstract)) -- , Either [Clause] [TClause]))

-- fetches the function signatures that are defined within the statement
definedFuncSign	:: Module -> TypeLookupTable -> FQN -> Statement -> Exc [FunctionInfo]
definedFuncSign m tlt fqn (FunctionStm function)
	= do	defs	<- signs function |+> resolveSignature tlt fqn	:: Exc [Signature]
		visibs  <- defs |+> getVisibility m (visibility function)
		zip defs visibs |> (\(sign, vis) -> FI sign vis False Nothing (Just $ Left $ clauses function) ) & return

definedFuncSign m tlt fqn (ADTDefStm adtDef)
	= do	let definedType	= RNormal fqn (adtName adtDef)	:: RType
		fi	        <- adtDefinedFunctions tlt fqn adtDef	:: Exc [(Signature, Visible, Either [Clause] [TClause])]
		let wrapped	= fi |> (\(sign, vis, clauses) -> FI sign vis True (Just definedType) (Just clauses))	:: [FunctionInfo]
		return wrapped

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
		let sign	= Signature fqn ("as"++nm) ([funcT], funcReqs)
		return [FI sign Private True Nothing Nothing] -- TODO add implementation of "asSubType"

definedFuncSign m tlt fqn (ClassDefStm cd)
	= do	defType	<- resolveType tlt (Normal [] $ name cd)
		let defFree	= take (length $ frees cd) $ defaultFreeNames
		let defType'	= applyTypeArgsFree defType defFree
		signs	<- decls cd |+> resolveSignature' tlt fqn
		repeat (Public, False, Just defType') & zip signs
			|> (\(sign, (fiVis, fiGen, fiAbs)) -> FI sign fiVis fiGen fiAbs Nothing)
			& return

definedFuncSign _ _ _ _
	= return []
