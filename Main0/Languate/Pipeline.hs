module Languate.Pipeline where

{--

This module implements the pipeline, where everythin is loaded step by step from bnf, to package, typedpackage, example checked

--}

import Bnf
import Languate.AST
import Languate.TypedLoader
import Languate.File2Package
import Languate.FQN
import Languate.TypedPackage
import Languate.Interpreter.EvalExpr (evalExpr)
import Languate.Interpreter.Application (multApply)
import Languate.CheckExamples
import Languate.InterpreterDef
import Languate.ReadEvalPrint

import Data.Maybe
import Data.Map as Map

import Control.Monad.Reader

bnfPath	= "Parser/bnf/Languate"
project	= "workspace/Data/src/"

fqpn		= fromJust $ toFQPN "pietervdvn:Data"
prelude		= fromJust $ toFqn' fqpn [] "Prelude"
bool		= fromJust $ toFqn' fqpn ["Data"] "Bool"


doAllStuff	:: IO (TypedPackage, World)
doAllStuff	= do	bnfs	<- Bnf.load bnfPath
			package	<- loadPackage' bnfs prelude project
			
 			let tpack	= normalizePackage $ typeCheck fqpn package
			let evalf	= eval' tpack
			let exampleErrs	= checkModules evalf package
			putStrLn exampleErrs
			return (tpack,bnfs)


eval'		:: TPackage -> FQN -> Expression -> Value
eval' tpack fqn expr
		= let tmod = fromMaybe (error $ "Module not found: "++show fqn) $  Map.lookup fqn tpack in
			let texpr	= convExpr tmod expr in
				runReader (evalExpr multApply texpr) (Context tpack fqn [])
