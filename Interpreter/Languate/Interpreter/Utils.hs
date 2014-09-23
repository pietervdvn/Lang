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
import Data.Map hiding (filter, map)
import Data.Maybe
import Control.Monad.Reader


mergeBindings	:: [ Binding ] -> Binding
mergeBindings 	=  merge . concatMap unmerge



-- searches, within the current context the clauses which correspond with given signature
search	:: Signature -> RC Value
search sign@(Signature name t)
	= do	ftcs		<- searchGlobal sign
		let foundMod	= fmap Lambda ftcs 
		local		<- ask' bindings
		let foundLocal	= lookup name local
		let found	= firstJust foundLocal foundMod
		let err0	= error ("Nothing found with signature " ++ show sign ++ _errStr)
		return $ fromMaybe err0 found
	
-- searches within the module (not within the bindings)	
searchGlobal	:: Signature -> RC (Maybe [TClause])
searchGlobal sign
	= do	w	<- ask' world
		fqn	<- ask' country
		let err	= error ("No module found with name "++show fqn++_errStr)
		let tcs	=  typedClauses $ findWithDefault err fqn w
		let foundMod	= lookupSt sign tcs 
		return foundMod

_errStr	= "\nThis is probably an interpreter invocation error, check inputs"

ask'	:: (r -> a) -> Reader r a
ask' f	=  do	r	<- ask
		return $ f r

-- executes the given reader in the slightly modified reader
setBindings	:: SymbolTable Value -> RC a -> RC a
setBindings bindings r
		=  do	(Context w c _) 	<- ask
			return $ runReader r (Context w c bindings)

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

isCurry (Curry _) 	= True
isCurry _		= False
