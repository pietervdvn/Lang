module Languate.FunctionTypeChecker (checkClause, TClause (TClause), precheck) where

{--

This module the function type checker. It tests wether each clause is of the given type.

--}

import StdDef
import Languate.AST
import Languate.TAST
import Languate.PatternTypeChecker
import Languate.TypeChecker
import Languate.TypeTable
import Data.Map (Map)
import qualified Data.Map as Map
import Languate.SymbolTable
import Control.Monad.Reader
import Debug.Trace
import Languate.Precedence.Precedence

-- checks the clause on missing references. We assume all functions have a known type
checkClause	:: TypeTable -> PrecedenceTable -> Type -> Clause -> TClause
checkClause tt prior targetType (Clause patterns expr)
		= _checkClause tt prior targetType patterns expr



_checkClause	:: TypeTable -> PrecedenceTable -> Type -> [Pattern] -> Expression -> TClause
_checkClause tt prior (Curry tps) ptrs expr
		= let (checkedPtrs, closure)	= checkPattern (tt, prior) ptrs (init' tps) in
		  let tt'	= TT tt $ Map.map (:[]) closure in
		  let expr'	= expr2prefExpr prior $ precheck tt' $ remNl expr in
		  let texpr	= runReader (typeCheck expr') tt in
		  let texpr'	= checkReturnType (last tps) texpr in
		  TClause checkedPtrs texpr'
_checkClause tt prior t ptrs expr
		= _checkClause tt prior (Curry [t]) ptrs expr


-- checks wether the expression contains a valid return type, crashes if a mismatch is found
checkReturnType	:: Type -> TExpression -> TExpression
checkReturnType expected actual
		= let possible	= typeOf actual in
		  if expected `elem` possible then actual
			else error $ "No possible type found for "++show actual++"; expected something of type "++show expected


-- does each variable exist? Crashes if not
precheck	:: TypeTable -> Expression -> Expression
precheck tt (Seq exprs)
		= Seq $ map (precheck tt) exprs
precheck tt (Tuple exprs)
		= Tuple $ map (precheck tt) exprs
precheck tt (Call str)
		= Call $ doesExist str tt
precheck tt (Operator str)
		= Operator $ doesExist str tt
precheck _ e	= e


doesExist	:: String -> TypeTable -> String
doesExist str tt
		= case findType str tt of
			Nothing	-> error $ "FunctionTypechecker: Unkown variable/function: "++show str++"\n"++show tt
			_	-> str
