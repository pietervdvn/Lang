module Languate.Pipeline where

{--

This module implements the pipeline, where everythin is loaded step by step from bnf, to package, typedpackage, example checked

--}

import Bnf
import Languate.AST
import Languate.TAST (typeOf, TypedExpression)
import Languate.File2Package
import Languate.FQN
import Languate.TypedPackage
import Languate.TypedLoader
import Languate.Interpreter.EvalExpr (evalExpr)
import Languate.Interpreter.Application (multApply)
import Languate.CheckExamples
import Languate.InterpreterDef
import Languate.ReadEvalPrint
import Languate.Precedence.Precedence

import Data.Maybe
import Data.Map as Map

import Control.Monad.Reader
import Control.Exception (catch, SomeException)

bnfPath	= "Parser/bnf/Languate"
project	= "workspace/Data/src/"

fqpn		= fromJust $ toFQPN "pietervdvn:Data"
prelude		= fromJust $ toFqn' fqpn [] "Prelude"
bool		= fromJust $ toFqn' fqpn ["Data"] "Bool"


doAllStuff	:: IO (TypedPackage, World, PrecedenceTable)
doAllStuff	= do	bnfs	<- Bnf.load bnfPath
			package	<- loadPackage' bnfs prelude project
			let precTable	= buildPrecTable $ elems package
 			let tpack	= normalizePackage $ typeCheck precTable fqpn package
			let evalf	= eval' tpack precTable
			let typeOfF	= typeOf' tpack precTable
			let exampleErrs	= checkModules evalf typeOfF package
			catch (putStrLn exampleErrs) hndl
			return (tpack,bnfs,precTable)
				where 	hndl	:: SomeException -> IO ()
					hndl msg	= putStrLn $ "Woops! Something went wrong with testing!\n"++show msg


typeOf'		:: TPackage -> PrecedenceTable -> FQN -> Expression -> Type
typeOf' tpack precT fqn expr
		= let texpr	= typeExpr tpack precT fqn expr in
		  	head $ typeOf texpr

eval'		:: TPackage -> PrecedenceTable -> FQN -> Expression -> Value
eval' tpack precT fqn expr
		= let texpr	= typeExpr tpack precT fqn expr in
			runReader (evalExpr multApply texpr) (Context tpack fqn [])

typeExpr	:: TPackage -> PrecedenceTable -> FQN -> Expression -> TypedExpression
typeExpr tpack precT fqn expr
		=  let tmod = fromMaybe (error $ "Module not found: "++show fqn) $  Map.lookup fqn tpack in
		   	convExpr tmod $ expr2prefExpr precT expr
