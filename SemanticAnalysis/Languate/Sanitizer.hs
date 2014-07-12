module Languate.Sanitizer where

import StdDef
import Languate.AST

{- Sanitize cleanses an expression: NL's get removed.
-}

sanitize	:: Expression -> Expression
sanitize priors (Tuple exprs)
		= Tuple $ cleanse priors exprs
sanitize priors (Seq exprs)
		= Seq $ cleanse priors exprs
sanitize _ e	= e



-- removes nl's, sanitizes
cleanse		:: Map Name Int -> [Expression] -> [Expression]
cleanse	priors	=  map (sanitize priors) . filter (not . isExprNl) 


isExprNl	:: Expression -> Bool
isExprNl (ExprNl _)	= True
isExprNl _		= False
