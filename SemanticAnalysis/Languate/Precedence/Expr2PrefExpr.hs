module Languate.Precedence.Expr2PrefExpr (expr2prefExpr) where

{- This module converts an AST.Expression into an AST.expression where all operator invocations are replaced by a prefix call notation, according to the precedence.

> True && False = (&&) True False
> !True && !False	= (&&) ((!) True) ((!) False)
> !True * False + False	= (+) ((*) ((!) True) False) False
etc...

How do we do this? First, we split the flat expression ''1 + 1 * 2'' into parts. We get the lowest class, and recursively bring the part left and right into prefix form.

-}

import StdDef
import Normalizable

import Languate.AST
import Languate.Precedence.PrecedenceTableDef
import Data.Maybe
import Prelude hiding (lookup)
import Data.Map hiding (filter, map)

import Languate.BuiltIns

expr2prefExpr	:: PrecedenceTable -> Expression -> OperatorFreeExpression
expr2prefExpr t (Operator nm)
		= Call nm
expr2prefExpr t e
		= expr2prefExpr' t e


-- internally used expr2prefExpr
expr2prefExpr'		:: PrecedenceTable -> Expression -> OperatorFreeExpression
expr2prefExpr' t (Seq exprs)
			= normalize $ makePref t $ map (expr2prefExpr' t) exprs
expr2prefExpr' t (Tuple exprs)
			= Tuple $ map (expr2prefExpr' t) exprs
expr2prefExpr' _ (Operator name)
			= Operator name	-- yes, this lines does nothing. It is imortant that operators -even lonely ones- stay operators. It breaks thing in the recursive call when it gets normalized into a (Call nm). Only the root call should do that!
expr2prefExpr' _ e	= e

makePref	:: PrecedenceTable -> [Expression] -> Expression
makePref tb exprs
	| not $ any isOperator exprs
	 	=  Seq exprs
	| otherwise
		= mp tb exprs

mp	:: PrecedenceTable -> [Expression] -> Expression
mp pt@(PrecedenceTable _ op2i i2op modifs _) exprs
	= let index	= minimum $ map (`precedenceOf` pt) $ filter isOperator exprs in		-- minimum index = should be executed as last
	  let mode	= modeOf index pt in
	  let seq	= filter (Seq [] /= ) $ map normalize $ splitSeq pt index exprs in
		bringInPrefix mode seq

{-
Split sec is more or less the core of the algorithm.
It takes the first n expressions, where no operator of level i exists. This block will be passed to a new makePref.
A list formed as [Expression, Operator, Expression, Operator, ...] will be returned, which is ready to place in prefix notation.
-}
splitSeq	:: PrecedenceTable -> Int -> [Expression] -> [Expression]
splitSeq pt i exprs
		= let (init, tail)	= break (\e -> i == precedenceOf e pt) exprs in
		  let head	= makePref pt init in
		  let tail'	= reSplitSeq pt i tail in
			head:tail'

reSplitSeq	:: PrecedenceTable -> Int -> [Expression] -> [Expression]
reSplitSeq pt i []	= []
reSplitSeq pt i (op@(Operator _):tail)
		= op:splitSeq pt i tail
reSplitSeq pt i seq
		= error $ "Languate/Precedence/Expr2PrefExpr.hs:reSplitSeq: falltrough on "++show seq++" with precedence "++show i


bringInPrefix	:: PrecModifier -> [Expression] -> Expression
bringInPrefix _ [expr]	= expr
bringInPrefix PrecPrefix (Operator nm:exprs)
		= let expr =	bringInPrefix PrecPrefix exprs in
			Seq [Call nm, expr]
bringInPrefix PrecPrefix exprs	= error $ "Prefix usage on more than one argument: "++show exprs
bringInPrefix PrecLeft (e1:Operator nm:e2:rest)
		= bringInPrefix PrecLeft ( Seq [Call nm, e1, e2] : rest)
bringInPrefix PrecPostfix (expr:Operator nm:rest)
		= bringInPrefix PrecPostfix $ Seq [Call nm, expr]:rest
bringInPrefix PrecRight (e1:Operator nm: rest)
		= let e2	= bringInPrefix PrecRight rest in
			Seq [Call nm, e1, e2]
bringInPrefix _ [e, Operator nm]
		= Seq [Call nm, e]
bringInPrefix _ [Operator nm, e]
		= Seq [Call $ flipSign & snd, Call nm, e]
bringInPrefix _ []
		= Seq []
bringInPrefix mod es
		= error $ "Error while converting the expression into prefix form: "++ (es |> show & unwords)
