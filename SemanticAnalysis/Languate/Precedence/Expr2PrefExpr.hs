module Languate.Precedence.Expr2PrefExpr where

{- This module converts an AST.Expression into an AST.expression where all operator invocations are replaced by a prefix call notation, according to the precedence.

> True && False = (&&) True False
> !True && !False	= (&&) ((!) True) ((!) False)
> !True * False + False	= (+) ((*) ((!) True) False) False
etc...
-}

import Languate.AST
import Languate.Precedence.PrecedenceTable

expr2prefExpr	:: PrecedenceTable -> Expression -> Expression
expr2prefExpr table e	= e
