module Pt2Expr where

{--
This module implements a simple expression parser, as seen in the readme
--}

import StdDef	-- needed for 'Name'
import Def	-- standard definitions
import Bnf	-- bnf lib
import Bnf.ParseTree	-- needed for parsetree
import Bnf.Converter	-- needed for errors and simpleconvert

import Control.Monad.Writer



data AST	= Value Int
		| Ident Name
		| Plus AST AST
		| PlusE AST
		| PlusT
	deriving (Show)

t       :: Name -> String -> AST
t _ "+"     =  PlusT
t "int" i   = Value $ read i    -- read is a builtin function that parses int
t "localIdent" name
        = Ident name
t name str	= error $ "Missed expression case in t "++ show name++" "++show str

s       :: Name -> [AST] -> AST
s "expr" [expr1, PlusE expr2]
        = Plus expr1 expr2
s "expr" [PlusT, expr]
	= PlusE expr
s _ [expr]  = expr
s name ast	= error $ "Missed expression case "++show name++" " ++ show ast


parseExpr  	 :: ParseTree -> Writer Errors Expr
parseExpr pt    =  do   parsed  <- simpleConvert (const $ const Nothing) t s pt
			let conved  = conv parsed
			    -- eventually, we can check certain properties here and issue a warning/error with tell. See Control.Monad.Writer docs
			return conved

conv        :: AST -> Expr
conv (Value i)  = Integer i -- let's i
conv (Ident nm) = Call nm
conv (Plus e f) = Add (conv e) (conv f)
