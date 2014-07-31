module Languate.ExportBuilder (buildExports) where

{--

This module calculates the exports 

--}

import StdDef
import State
import Languate.FQN
import Languate.File2Package
import Languate.AST hiding (Imports)
import Languate.Signature
import Data.Map hiding (map, filter, null)
import qualified Data.Map as Map
import Data.Either
import Data.Maybe
import Control.Monad
import Data.List (sort, nub)

import Debug.Trace

buildExports	:: FQPN -> Map FQN Module -> Map FQN [Signature] -> Map FQN [(FQN,Signature)]
buildExports fqnp modules localDeclared
		=  let imps	= Map.map (extractImports fqnp) modules in
		   let invImps	= buildReverseImportGraph fqnp modules in
		   let ctx	= Context localDeclared imps invImps modules in
		   let state	= (ctx, keys modules, empty) :: St in
		   let (_,(_,_,exports))
				= runstate buildExp state in
			exports



type ImportedBy	= Map FQN [FQN]
type Imports	= Map FQN [(FQN, Visible, Restrict)]
type LocallyDeclared	= Map FQN [Signature]
-- locally declared signatures, which module is imported by which, the raw data
data Context	= Context {locallyDeclared::LocallyDeclared, imps::Imports, invImps::ImportedBy, raw::Map FQN Module}
type Todo	= FQN
type Exports	= Map FQN [(FQN, Signature)]
type St		= (Context, [Todo], Exports)
-- context: some usefull data; todo=these should still be (re)calculated, exports: what we need

-- ## Main algo


-- builds all the exports; uses a state algorithm
buildExp	:: State St ()
buildExp	=  do	todo	<- getTodo
			unless (null todo) $
				do	let next	= head todo
					modTodo tail
					buildExpFor next


buildExpFor	:: FQN -> State St ()
buildExpFor fqn	=  do	local	<- getLocalProduce fqn
			imports	<- getImports fqn
			exports	<- mapM getExports' imports
			-- sorting to normalize
			let exports'	= sort $ (zip (repeat fqn) local ++ concat exports)	
			-- we now have the exports of this fqn in the current state!
			-- if these have changed, we have to re-evaluate the fqns which import this fqn
			current	<- getExports fqn
			when (exports' /= current)
				(do	setExports fqn exports'
					-- update todolist, all modules which export this might be changed
					impBy	<- getInvImports fqn
					modTodo $ nub . sort . (impBy++))
			buildExp


					
-- ### misc functions

mask	::  Restrict -> [Signature] ->[Signature]
mask (BlackList [])	=  id
mask (BlackList forbidden)
	= filter (\(Signature name _) -> not $ name `elem` forbidden)
mask (WhiteList allowed)
	= filter (\(Signature name _) -> name `elem` allowed)

mask'	:: Restrict -> [(FQN, Signature)] -> [(FQN, Signature)]
mask' (BlackList [])	= id
mask' (BlackList forbidden)
			= filter (\(_,Signature name _) -> not $ name `elem` forbidden)
mask' (WhiteList allowed)
			= filter (\(_,Signature name _) -> name `elem` allowed)


-- ### Building of reverse imports 


-- builds a 'FQN does import these [FQN]'-relationship
importTable		:: FQPN -> Map FQN Module -> Map FQN [FQN]
importTable fqnp	=  Map.map (extractImports' fqnp)

extractImports'	fqnp	=  map (\(fqn,_,_) -> fqn) . extractImports fqnp

extractImports 		:: FQPN -> Module -> [(FQN, Visible, Restrict)]
extractImports fqnp	= map (\imp@(Import v _ _ r) -> (import2fqn fqnp imp, v, r)) . rights . imports

buildReverseImportGraph	:: FQPN -> Map FQN Module -> Map FQN [FQN]
buildReverseImportGraph fqnp
	= buildReverseImportGraph' . importTable fqnp


-- gives a 'FQN is imported by [FQNS]'-relationship
buildReverseImportGraph':: Map FQN [FQN] -> Map FQN [FQN]
buildReverseImportGraph' mp
	= fromList $ merge [ (fqn, fqn') | (fqn, fqns) <- toList mp, fqn' <- fqns]



-- ### accessor functions for making the main algo more readable
getTodo	:: State St [FQN]
getTodo	=  do	(_, todo, _)	<- get
		return todo

modTodo		:: ([FQN] -> [FQN]) -> State St ()
modTodo f	=  do	(a,todo,b)		<- get
			put (a, f todo, b)


getLocalProduce	:: FQN -> State St [Signature]
getLocalProduce fqn
		=  do	ld	<- fmap locallyDeclared getContext
			extract "locallyDeclared" fqn ld

-- gets all fqns which this fqn imports
getImports	:: FQN -> State St [(FQN, Visible, Restrict)]
getImports fqn	=  do	impMap	<- fmap imps getContext
			extract "imports" fqn impMap

-- gets all the signatures which are currently exported
getExports	:: FQN -> State St [(FQN, Signature)]
getExports fqn	=  do	(_,_,exprs)	<- get
			return $ fromMaybe [] $ Map.lookup fqn exprs

setExports	:: FQN -> [(FQN, Signature)] -> State St ()
setExports fqn exps
		=  do	(a,b,expMap)		<- get
			put (a,b,insert fqn exps expMap)


-- same as getExports, but with mask. This means that import modifiers are taken into account (public/private; hiding/showing)
getExports'	:: (FQN, Visible, Restrict) -> State St [(FQN, Signature)]
getExports' (_,Private,_)
		= return []
getExports' (fqn, Public, restrictions)
		= do	exps	<- getExports fqn
			return $ mask' restrictions exps


-- gets all fqns into which this fqn was imported
getInvImports	:: FQN -> State St [FQN]
getInvImports fqn
		=  do	invImpMap	<- fmap invImps getContext
			extract "invImports" fqn invImpMap


getContext	:: State St Context
getContext	=  do	(ctx,_,_)	<- get
			return ctx

extract ctx fqn dict
		= return $ fromMaybe (error $ errMsg ctx fqn) $ Map.lookup fqn dict

errMsg ctx fqn	= "ExportBuilder: fqn "++show fqn++" not found in the contextmaps (of "++ctx++"); this is a bug"
