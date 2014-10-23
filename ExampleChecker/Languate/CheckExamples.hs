module Languate.CheckExamples where

{--

This module implements the example checker.
--}

import Control.Monad

import Languate.AST
import Data.List
import Data.Map (mapWithKey, toList, Map)
import Languate.FQN

import Debug.Trace

checkExample 	:: Eq a => (Expression -> a) -> Expression -> Expression -> Bool
checkExample eval e1 e2
		= eval e1 == eval e2

checkType	:: Eq t => (Expression -> t) -> Expression -> Expression -> Bool
checkType	=  checkExample

genMsg		:: Expression -> Expression -> String
genMsg e1 e2	=  "These expressions do not evaluate to the same value: "++show e1++" != "++show e2

getTypeMsg	:: Show t => (Expression -> t) -> Expression -> Expression -> String
getTypeMsg typeOf e1 e2
		=  "These expressions do not have the same type:"++showOne e1++showOne e2
			where	showOne	e = "\n\t" ++ show e ++" : "++show (typeOf e)

-- empty string == no examples!
checkExamples	:: (Eq a, Eq t, Show t) => (Expression -> a) -> (Expression -> t) -> [Law] -> [String]
checkExamples eval typeOf laws
		= do	Example _ e1 e2 	<- filter isExample laws
			let checkedEx	= return (if checkExample eval e1 e2 then [] else genMsg e1 e2)
			if checkType typeOf e1 e2 then checkedEx else return $ getTypeMsg typeOf e1 e2

checkModule	:: (Eq a, Eq t, Show t) => (Expression -> a) -> (Expression -> t) ->  Module -> [String]
checkModule eval typeOf mod
		=  let laws 	= getLaws $ statements mod in
			checkExamples eval typeOf laws

checkModule' eval typeOf mod
		= let msgs	= checkModule eval typeOf mod in
			if null msgs then "" else  "\nChecking examples in " ++ moduleName mod++":\n " ++ intercalate "\n " (filter ("" /=) msgs)

checkModules	:: (Eq a, Eq t, Show t) => (FQN -> Expression -> a) -> (FQN -> Expression -> t) ->  Map FQN Module -> String
checkModules eval typeOf
		= concatMap (\(fqn, mod) -> checkModule' (eval fqn) (typeOf fqn) mod) . toList



isExample	:: Law -> Bool
isExample Example{}
		= True
isExample _	= False

getLaws		:: [Statement] -> [Law]
getLaws []	=  []
getLaws (ExampleStm l:rest)
		=  l:getLaws rest
getLaws (FunctionStm f:rest)
		= laws f ++ getLaws rest
getLaws (_:rest)	= getLaws rest

isLaw		:: Statement -> Bool
isLaw (ExampleStm _)
		= True
isLaw _		= False
