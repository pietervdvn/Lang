module Pt2Stmt where

{--

This module parses statements of the simple, straightline program syntax

--}

import StdDef -- needed for 'Name'

import Def
import Bnf
import Bnf.ParseTree
import Bnf.Converter

import Pt2Expr (parseExpr)

import Control.Monad.Writer


data AST    	= Print Expr
		| Assign Name Expr
		| Expr Expr
		| PrintT
		| AssignT
		| Ident Name
	deriving (Show)

h       :: Name -> ParseTree -> Maybe (Writer Errors AST)
h "expr" pt = Just $ fmap Expr $ parseExpr pt
h _ _       = Nothing

t       :: Name -> String -> AST
t _ ":="    =  AssignT
t _ "print" =  PrintT
t "localIdent" name
        =  Ident name

s       :: Name -> [AST] -> AST
s "statement" [PrintT, Expr e]
        =  Print e
s "statement" [Ident nm, AssignT, Expr expr]
        =  Assign nm expr
s _ [ast]
	= ast
s name ast	= error $ "Missed statement case "++show name++" " ++ show ast

conv        :: AST -> Statement
conv (Print expr)
        = PrintStmt expr
conv (Assign name expr)
        = AssignStmt name expr

parseStmt   :: ParseTree -> Writer Errors Statement
parseStmt pt    =  do   parsed  <- simpleConvert h t s pt
            		return $ conv parsed
