module Def.Parser.Pt2Expr (pt2expr) where

import StdDef
import Bnf.ParseTree
import Bnf hiding (SimpleConvert)
import Control.Monad.Writer
import Control.Monad
import Def.Parser.Utils
import Def.Parser.Pt2Prelude
import Def.Parser.Pt2Type
import qualified Def.AST as Def
import Def.AST hiding (Seq, Flt, Nat, Chr, List, Tuple)

{--

This module converts the ParseTree (of an expression) into a Expression, via an AST

--}

pt2expr		:: ParseTree -> Expression
pt2expr		=  pt2a h t s convert

convert		:: AST -> Expression
convert (FC id)	=  Call id
convert (Op id)	=  Operator id
convert (Chr c)	=  Def.Chr c
convert (Str s)	=  desugareString s
convert (Nat i)	=  Def.Nat i
convert (Flt f)	=  Def.Flt f
convert (List asts)
		=  desugareList $ map convert asts;
convert (Set asts)
		=  Def.Seq [BuiltIn "toSet", convert $ List asts]
convert (Dict asts)
		= Def.Seq [BuiltIn "toDict", convert $ List asts]
convert (Tuple asts)
		= Def.Tuple $ map convert asts
convert (Seq asts)
		= Def.Seq $ map convert asts
convert (Cst t)	= Cast t
convert AutoCst	= AutoCast
convert ast	= convErr "Pt2Expr" ast


desugareString	:: String -> Expression
desugareString s
		= Def.Seq [BuiltIn "toString", desugareList $ map Def.Chr s]

desugareList	:: [Expression] -> Expression
desugareList []	=  BuiltIn "empty"
desugareList (exp:exprs)
		=  Def.Seq [ BuiltIn "prepend", exp , desugareList exprs]

data AST	= FC String	-- A Function Call to a constructor (globalIdent), function (localIdent)
		| Op String	-- A call to an infix op
		| Chr Char
		| Str String
		| Nat Int
		| Flt Float
		| ParO	| ParC	-- ( )
		| SetO	| SetC	-- { }
		| ListO	| ListC	-- [ ]
		| DictArrow	-- --> in a dictionary
		| Comma
		| CommaSepExpr [AST]
		| List 	[AST]
		| Set 	[AST]
		| Dict 	[AST]
		| Tuple [AST]
		| Seq 	[AST]
		| AutoCst
		| Cst Type
		| CstT	-- open cast, ~(
	deriving (Show)


h		=  	[ ("char",	Chr . parseChar)
			, ("string", 	Str . parseString)
			, ("nat",	Nat . parseNat)
			, ("float",	Flt . parseFloat)
			, ("baseType", 	Cst . pt2type)]

t		:: Name -> String -> AST
t "localIdent" id	= FC id
t "globalIdent" id	= FC id
t "op" id		= Op id
t _ "("			= ParO
t _ ")"			= ParC
t _ "{"			= SetO
t _ "}"			= SetC
t _ "["			= ListO
t _ "]"			= ListC
t _ ","			= Comma
t _ "~~"		= AutoCst
t _ "~"			= CstT
t _ "-->"		= DictArrow
t nm cont	=  tokenErr "Pt2Expr" nm cont


s		:: Name -> [AST] -> AST
s "commaSepExpr" [Comma, expr]
		= expr
s "commaSepExpr" [expr, CommaSepExpr exprs]
		= CommaSepExpr $ expr:exprs
s "commaSepExpr" exprs
		= CommaSepExpr exprs

s "commaSepExprP" [Comma, expr]
		= expr
s "commaSepExprP" [expr, CommaSepExpr exprs]
		= CommaSepExpr $ expr:exprs
s "commaSepExprP" exprs
		= CommaSepExpr exprs

s "arrowTuple" [expKey, DictArrow, expValue]
		= Tuple [expKey, expValue]
s "dictCont" [Comma, arrTuple]
		= arrTuple
s "dictCont" [ast, CommaSepExpr tuples]
		= CommaSepExpr $ ast:tuples
s "dictCont" tuples
		= CommaSepExpr tuples
s "dict" [SetO, tuples, SetC]
		= Dict $ unpack tuples

s "list" [ListO, exprs, ListC]
		= List $ unpack exprs
s "set" [SetO, exprs, SetC]
		= Set $ unpack exprs
s "tuple" [ParO, exprs, ParC]
		= Tuple $ unpack exprs
s "simpleExpr" [ParO, expr, ParC]
		= expr
s "simpleExpr" [CstT, Cst typ]
		= Cst typ
s "expr" [exp]	= exp
s "expr" exprs	= Seq exprs
s _ [expr]  = expr
s nm exprs	= seqErr "Pt2Expr" nm exprs

unpack		:: AST -> [AST]
unpack (CommaSepExpr asts)	
		= asts
unpack ast	= [ast]
