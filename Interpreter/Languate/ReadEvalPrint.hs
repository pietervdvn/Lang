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
import Languate.TypeChecker (typeCheck, Context (Context))
import Languate.TypedLoader (priorTable)	-- the hardcoded prioritytable
import Languate.TypeTable
import Languate.FunctionTypeChecker (precheck)
import Languate.InterpreterDef
import Languate.Interpreter.EvalExpr
import Languate.Interpreter.Application (multApply, eval)

import Data.Maybe

import Control.Monad.Reader
import Data.Map

parseEval	:: World -> TPackage -> FQN -> String
  -> Value
parseEval w tp fqn str
		= let 	texpr	= parseTypedExpr w tp fqn str in
			runReader (evalExpr multApply texpr >>= eval) $ Languate.InterpreterDef.Context tp fqn []
			

parseTypedExpr	:: World -> TPackage -> FQN -> String -> TExpression
parseTypedExpr w tpack fqn str
	 	=  	let expr	= parseExpr w str in
			let tmod	= findWithDefault errM fqn tpack in
			_convExpr tmod expr
			where errM	= error $ "The module "++ show fqn++" was not found in the current index"

parseExpr	:: World -> String -> Expression -- as AST.Expression, untyped
parseExpr w str	=  let pt = parse w (Bnf.toFQN ["Expressions"]) "expr" str in 
		 	pt2expr $ either (errM str) id $ fromMaybe (errM str) pt


_convExpr	:: TModule -> Expression -> TypedExpression
_convExpr tmod expr	
		=  let typeTable	= _constrTypeTable tmod in
		   let expr'		= precheck typeTable expr in
			runReader (typeCheck expr') $ Languate.TypeChecker.Context typeTable priorTable

_constrTypeTable	:: TModule -> TypeTable
_constrTypeTable tmod	=  buildTypeTable $ typedClauses tmod

errM str	= error $ "Parse error: we could not parse "++str
