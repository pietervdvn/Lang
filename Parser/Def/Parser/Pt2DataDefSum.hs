module Def.Parser.Pt2DataDefSum (pt2sum) where

import StdDef
import Bnf.ParseTree
import Bnf
import Control.Monad.Writer
import Control.Monad
import Def.Parser.Utils
import Def.Parser.Pt2Prelude
import Def.Parser.Pt2Type
import Def.Parser.Pt2Expr
import Def.Parser.Pt2Pattern
import Def.Parser.Pt2Comment
import Def.AST

{--

This module converts the ParseTree into a constructor-sum.
It parses stuff as

Sum Bool Int

in a statement like

data ADT	= Sum Bool Int
		| Prod 

See comments in the bnf for exact syntax and docstring locations.

This part is not responsible for adding docstrings.

Implementation is split in a part which parses a namedSum 'a,b,c:Int', and a part which parses an entire part.
--}

modName	= "Pt2DataDefSum"

pt2sum	:: ParseTree -> ADTSum
pt2sum	=  pt2a h t s convert . cleanAll ["nlt","comma"]

convert		:: AST -> ADTSum
convert (Priv ast)
		= let ADTSum name _ comm types = convert ast in
			ADTSum name Private comm types
convert (UnNamedConstr name types)
		= ADTSum name Public Nothing $ zip (repeat Nothing) types
convert (Constr name)
		= ADTSum name Public Nothing []
convert (NamedConstr name names)
		= ADTSum name Public Nothing names
convert ast	= convErr modName ast


data AST	= Ident Name
		| Constr Name
		| UnNamedConstr Name [Type]
		| Names [(Maybe Name, Type)]
		| NamedConstr Name [(Maybe Name, Type)]
		| Types [Type]
		| Type Type
		| Priv AST
		| PrivateT
	deriving (Show)


h		:: [(Name, ParseTree -> AST)]
h		=  [("baseType",Type . pt2type),("namedSum",Names . pt2nsum)]

t		:: Name -> String -> AST
t "constructor" id
		=  Constr id
t _ "_"		=  PrivateT
t nm cont	=  tokenErr modName nm cont


s		:: Name -> [AST] -> AST
s "types" asts	= Types $ map (\(Type t) -> t) asts
s "sum" (PrivateT:asts)
		= Priv $ s "sum" asts
s "sum" [Constr name, Types types]
		= UnNamedConstr name types
s "sum" [Constr name, Names names]
		= NamedConstr name names
s _ [ast]	= ast
s nm asts	= seqErr modName nm asts


-- Named-sum-parsing --
-----------------------


modNameN	= modName++"-Named-Sum"

pt2nsum	:: ParseTree -> [(Maybe Name, Type)]
pt2nsum	=  pt2a hn tn sn convertN . cleanAll ["comma"]

convertN	:: ASTN -> [(Maybe Name, Type)]
convertN (NamedSum stuff)
		= stuff
convertN ast	= convErr modNameN ast

data ASTN	= NamedSum [(Maybe Name, Type)]
		| IdentN Name
		| Idents [Name]
		| TypeN Type
		| ColonT
	deriving (Show)


hn		:: [(Name, ParseTree -> ASTN)]
hn		=  [("type",TypeN . pt2type)]

tn		:: Name -> String -> ASTN
tn "localIdent" id
		=  IdentN id
tn _ ":"	=  ColonT
tn nm cont	=  tokenErr modNameN nm cont


sn		:: Name -> [ASTN] -> ASTN
sn "idents" asts
		= Idents $ map (\(IdentN n) -> n) asts
sn _ [idents, ColonT]
		= idents
sn "optNamed" [TypeN t]
		= NamedSum [(Nothing, t)]
sn _ [Idents ids, TypeN t]
		= NamedSum $ zip (map Just ids) $ repeat t
sn _ asts@(NamedSum _:_)
		= NamedSum $ concatMap (\(NamedSum ls) -> ls) asts
sn _ [ast]	= ast
sn nm asts	= seqErr modNameN nm asts
