module Languate.AST.FunctionASTUtils where

{--

This module implements the utils for the function AST

--}
import StdDef
import Normalizable
import Data.List (intercalate)
import Languate.AST.FunctionAST
import Languate.AST.TypeAST
import Languate.AST.TypeASTUtils

instance Show Clause where
	show (Clause patterns expr)	= tabs 2 (unwords $ map show patterns)++"= "++show expr

isNl		:: Expression -> Bool
isNl (ExpNl _)	= True
isNl _		= False

remNl		:: Expression -> Expression
remNl (Seq es)	=  Seq $ remNls es
remNl (Tuple es)
		=  Tuple $ remNls es
remNl e		= e

remNls		=  filter (not . isNl)

instance Show Pattern where
	show 	= sp

sp		:: Pattern -> String
sp (Assign nm)	=  nm
sp (Let nm expr)
		=  nm++":=("++show expr++")"
sp (Deconstruct nm [])
		= nm
sp (Deconstruct nm patterns)
		= "("++nm++" "++ unwords (map show patterns)++")"
sp (Multi patterns)
		= intercalate "@" $ map show patterns
sp (Eval expr)	= '$':show expr
sp DontCare	= "_"
sp MultiDontCare
		= "*"

instance Normalizable Pattern where
	normalize	= np

np		:: Pattern -> Pattern
np (Deconstruct nm ptrns)
		= Deconstruct nm $ filter (Multi [] /=) $ nps ptrns
np (Multi [p])	= normalize p
np (Multi pts)	= Multi $ nps pts
np (Eval e)	= Eval e
np pt		= pt


nps	= map normalize

instance Show Function where
	show f	= "Function: docstr: "++(show $ docstr f)++" visibility: "++(show $ visibility f)++" signs: "++(show $ signs f)++" laws: "++(show $ laws f)++" clauses: "++(show $ clauses f)
