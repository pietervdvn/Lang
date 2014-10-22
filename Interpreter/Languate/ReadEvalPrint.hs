module Languate.ReadEvalPrint where

{--

This module implements a read-eval-print step

--}
import StdDef
import Bnf
import Languate.AST
import Languate.FQN
import Languate.ParserStub
import Languate.TAST
import Languate.TypedPackage
import Languate.TypeChecker (typeCheck	)
import Languate.TypeTable
import Languate.FunctionTypeChecker (precheck)
import Languate.InterpreterDef
import Languate.Interpreter.EvalExpr
import Languate.Interpreter.Application (multApply, eval)
import Languate.Precedence.Precedence

import Data.Maybe

import Control.Monad.Reader
import Data.Map

parseEval	:: World -> TPackage -> PrecedenceTable -> FQN -> String  -> Value
parseEval w tp precT fqn str
		= let 	texpr	= parseTypedExpr w tp precT fqn str in
			runReader (evalExpr multApply texpr >>= eval) $ Languate.InterpreterDef.Context tp fqn []


parseTypedExpr	:: World -> TPackage -> PrecedenceTable -> FQN -> String -> TExpression
parseTypedExpr w tpack precT fqn str
	 	=  	let expr	= parseExpr w precT str in
			let tmod	= findWithDefault errM fqn tpack in
			convExpr tmod expr
			where errM	= error $ "The module "++ show fqn++" was not found in the current index"

parseExpr	:: World -> PrecedenceTable -> String -> Expression -- as AST.Expression, untyped
parseExpr w precT str
		=  let pt = parse w (Bnf.toFQN ["Expressions"]) "expr" str in
		 	expr2prefExpr precT $ pt2expr $ either (errM str) id $ fromMaybe (errM str) pt


convExpr	:: TModule -> Expression -> TypedExpression
convExpr tmod expr
		=  let typeTable	= _constrTypeTable tmod in
		   let expr'		= precheck typeTable expr in
			runReader (typeCheck expr') typeTable

_constrTypeTable	:: TModule -> TypeTable
_constrTypeTable tmod	=  buildTypeTable $ typedClauses tmod

errM str	= error $ "Parse error: we could not parse "++str
