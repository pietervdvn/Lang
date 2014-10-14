module Languate.Interpreter.Utils where

{--

This module implements some interpreter utils, small misch functions which are usefull in the entire interpreter

--}

import StdDef
import Languate.InterpreterDef
import Languate.AST
import Languate.TAST
import Languate.SymbolTable
import Languate.Signature
import Languate.TypedPackage
import Data.Map hiding (filter, map, lookup)
import qualified Data.Map as Map
import Data.Maybe
import Control.Monad.Reader


-- searches first locally, then globally
search		:: Signature -> RC (Maybe Value)
search sign@(Signature name _)
		= do	local	<- searchLocal name
			if isJust local then return local
				else searchGlobal' sign

-- searches a name within the local bindings
searchLocal	:: Name -> RC (Maybe Value)
searchLocal nm	=  do	binds	<- ask' bindings
			return $ lookup nm binds

-- searches within the module (not within the bindings)	
searchGlobal	:: Signature -> RC (Maybe [TClause])
searchGlobal sign
	= do	w	<- ask' world
		fqn	<- ask' country
		let err	= error ("No module found with name "++show fqn++_errStr)
		let tcs	=  typedClauses $ findWithDefault err fqn w
		let foundMod	= lookupSt sign tcs 
		return foundMod

-- searches witing the module (not within the bindings) and wrap it as a lambda
searchGlobal'	:: Signature -> RC (Maybe Value)
searchGlobal' sign@(Signature nm t)
	= do	found	<- searchGlobal sign
		ctx	<- ask
		if isNothing found then return Nothing
			else do	let (Just clauses)	= found
				let clauses'		= map (\c -> (ctx,c)) clauses
				let (argT, retT)	= deCurry t
				return $ Just $ Lambda argT retT clauses'

_errStr	= "\nThis is probably an interpreter invocation error, check inputs"

ask'	:: (r -> a) -> Reader r a
ask' f	=  do	r	<- ask
		return $ f r

-- executes the given reader in the slightly modified reader
setBindings	:: Binding -> RC a -> RC a
setBindings bindings r
		=  do	(Context w c _) 	<- ask
			return $ runReader r (Context w c bindings)

setContext	:: Context -> RC a -> RC a
setContext ct r	= return $ runReader r ct

firstJust	:: Maybe a -> Maybe a -> Maybe a
firstJust (Just a) _	= Just a
firstJust _ a	= a

-- given: the possible types of an expression, the type this expression should return,
-- e.g. [Bool -> Int -> Bool, Int -> Int -> Int] -> Bool -> [Bool, Int] 
neededArgs	:: [Type] -> Type -> [Type]
neededArgs possible req
		= 	let possibleResults 	= map fst $ filter ((==) req . snd) $ map (\(Curry ts) -> (init ts, last ts)) $ filter isCurry possible in
			let l = length possibleResults in
			if l == 1 then head possibleResults
			else if l == 0 then error $ "No type found which results in a "++show req
			else error $ "To much types found which result in a "++show req

deCurry		:: Type -> ([Type], Type)
deCurry (Curry [t])
		= ([],t)
deCurry (Curry ts)
		= (init ts, last ts)
deCurry t	= ([],t)


isCurry (Curry _) 	= True
isCurry _		= False



repack	:: (a, Maybe b)	-> Maybe (a, b)
repack (a, Just b)	= Just (a,b)
repack _	= Nothing
