module Bnf.Meta.Pt2Rule where

import Bnf.Meta.Pt2BnfExpr (parseExpr)
import Bnf.ParseTree
import Bnf.BNF
import Bnf.Converter
import Control.Monad.Writer hiding (getFirst)
import StdDef
import Bnf.Meta.IOModule
import Normalizable
{--
This module implements the conversion from a pt to a bnf rule
--}

parseRule	:: ParseTree -> Writer Errors IORule
parseRule pt	=  do	conved	<- simpleConvert h t s pt
			conv (getInf $ getFirst pt) conved

conv		:: RuleInfo -> AST -> Writer Errors IORule
conv inf (Modded (Modifiers init priv token) ast)
		= do	IORule name expr _ _ pos	<- conv inf ast
			when (init && priv) $ tell [_initialNotPublWarn inf]
			let expr'	= if token then Token $ tokenize expr
						else expr
			return $ IORule name (normalize expr') (not priv) init pos
conv (_,_,(_,l,c)) (Rule name expr)
		= return $ IORule name  (normalize expr) True False (l,c)

_initialNotPublWarn inf
		= Left (inf, "The initial rule "++ show (getName inf) ++" is not public")

data AST	= Rule Name Expression
		| Modded AST AST
		| Ident Name
		| Expr Expression
		| AssignT
		| InitialMod
		| PrivateMod
		| TokenMod
		| Modifiers Bool Bool Bool	-- initial private tokenize
	deriving (Show)

h		:: Name -> ParseTree -> Maybe (Writer Errors AST)
h "expression" pt
		= Just (do	expr	<- parseExpr pt
				return $ Expr expr)
h nm pt		=  Nothing


t		:: Name -> String -> AST

t "localIdent" name	= Ident name

t "rule" "::="		= AssignT
t "rule" "="		= AssignT
t "modifier" ">"	= InitialMod
t "modifier" "_"	= PrivateMod
t "modifier" "$"	= TokenMod

t nm str	=  error $ "Token fallthrough for "++nm++" with "++show str



s		:: Name -> [AST] -> AST

s "modifier" []	= Modifiers False False False

s "modifier" (InitialMod:rest)
		= let Modifiers _ priv token 	= s "modifier" rest in
			Modifiers True priv token
s "modifier" (PrivateMod:rest)
		= let Modifiers init _ token 	= s "modifier" rest in
			Modifiers init True token
s "modifier" (TokenMod:rest)
		= let Modifiers init priv _ 	= s "modifier" rest in
			Modifiers init priv True

s _ [item]	= item
s "rule" (mods@Modifiers{}:rest)
		= Modded mods $ s "rule" rest 
s "rule" [Ident name, AssignT, Expr expr]
		= Rule name expr
s nm items	=  error $ "Seq fallthrough for "++nm++" with "++show items
