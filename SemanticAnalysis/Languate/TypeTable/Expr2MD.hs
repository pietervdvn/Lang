module Languate.TypeTable.Expr2MD where

{-Conversion of laws, expressions, ... into MD -}

import StdDef
import MarkDown
import Languate.AST

expr2md	:: Expression -> MarkDown
expr2md e	= code $ show e

law2md	:: Law -> MarkDown
law2md (Example mname e1 e2)
	= nm mname ++ equiv e1 e2
law2md (Law mname decls reqs e1 e2)
	= nm mname ++ commas (map decl2MD $ merge' decls) ++ typeReqs2MD reqs ++ equiv e1 e2

nm	:: Maybe Name -> MarkDown
nm Nothing	= ""
nm (Just nm)	= bold nm

equiv	:: Expression -> Expression -> MarkDown
equiv e1 e2
	= expr2md e1 ++ sndExpr e2

sndExpr	:: Expression -> MarkDown
sndExpr (Call "True")
	= ""
sndExpr e	= " = "++ expr2md e


decl2MD		:: ([Name], Maybe Type) -> MarkDown
decl2MD (n, Nothing)	= code $ commas $ reverse n
decl2MD (n, Just t)	= code $ commas (reverse n) ++ " : "++show t


typeReqs2MD	= (++ " ") . commas . fmap showTypeReq


bool	= Normal "Bool"
tl1	= Law (Just "operator precedence") [("d",Just bool),("c",Just bool),("b",Just bool),("a",Just bool)] [] (Call "True") (Call "True")
-- (a == b && c == d) ((a == b) && (c == d))
tl2	= Law Nothing [("b",Nothing),("a",Just bool)] [("a", Normal "Eq")] (Call "True") (Call "True")
-- (! a == b) ((! a) == b)][("==",eq -> (eq -> Bool),Nothing,[])]
