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

genMsg		:: Expression -> Expression -> String
genMsg e1 e2	=  "These expressions do not evaluate to the same value: "++show e1++" != "++show e2

-- empty string == all ok!
checkExamples	:: Eq a => (Expression -> a) -> [Law] -> [String]
checkExamples eval laws
		= do	Example e1 e2 	<- filter isExample laws
			if checkExample eval e1 e2 then return []
				else return $ genMsg e1 e2

checkModule	:: Eq a => (Expression -> a) -> Module -> [String]
checkModule eval mod	
		=  let laws 	= getLaws $ statements mod in
			checkExamples eval laws

checkModule' eval mod
		= let msgs	= checkModule eval mod in
			if null msgs then "" else  "\nChecking examples in " ++ moduleName mod++":\n " ++ (intercalate "\n " $ filter ((/=) "") msgs)

checkModules	:: Eq a => (FQN -> Expression -> a) -> Map FQN Module -> String
checkModules eval
		= concat . map (\(fqn, mod) -> checkModule' (eval fqn) mod) . toList 
				


isExample	:: Law -> Bool
isExample (Example _ _)
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


