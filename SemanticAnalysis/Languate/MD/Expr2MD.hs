module Languate.MD.Expr2MD where

{-Conversion of laws, expressions, ... into MD -}

import StdDef
import MarkDown
import Languate.AST
import Languate.TAST
import Languate.FQN

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


typeReqs2MD	:: [TypeRequirement] -> MarkDown
typeReqs2MD	= (++ " ") . commas . fmap showTypeReq

rtypeReqs2MD	:: [(Name, [RType])] -> MarkDown
rtypeReqs2MD	= (++ " ") . commas . fmap showRTypeReq'

showTypeId	:: (FQN, Name) -> MarkDown
showTypeId (fqn, n)
		= showShortFQN fqn ++ "." ++ n
