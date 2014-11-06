module Languate.Parser.Pt2DataDefSum (pt2sum) where

import StdDef
import Bnf.ParseTree
import Bnf
import Languate.Parser.Utils
import Languate.Parser.Pt2Type
import Languate.AST

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

pt2sum	:: ParseTree -> (ADTSum, [TypeRequirement])
pt2sum	=  pt2a h t s convert . cleanAll ["nlt","comma"]

convert		:: AST -> (ADTSum, [TypeRequirement])
convert (Priv ast)
		= let (ADTSum name _ comm types, reqs) = convert ast in
			(ADTSum name Private comm types, reqs)
convert (UnNamedConstr name types reqs)
		= (ADTSum name Public Nothing $ zip (repeat Nothing) types, reqs)
convert (Constr name)
		= (ADTSum name Public Nothing [], [])
convert (NamedConstr name names reqs)
		= (ADTSum name Public Nothing names, reqs)
convert ast	= convErr modName ast


data AST	= Ident Name
		| Constr Name
		| UnNamedConstr Name [Type] [TypeRequirement]
		| Names [(Maybe Name, Type)] [TypeRequirement]
		| NamedConstr Name [(Maybe Name, Type)] [TypeRequirement]
		| Types [Type] [TypeRequirement]
		| Type Type [TypeRequirement]
		| Priv AST
		| PrivateT
	deriving (Show)


h		:: [(Name, ParseTree -> AST)]
h		=  [("baseType",uncurry Type . pt2type),("namedSum", uncurry Names . pt2nsum)]

t		:: Name -> String -> AST
t "constructor" id
		=  Constr id
t _ "_"		=  PrivateT
t nm cont	=  tokenErr modName nm cont


s		:: Name -> [AST] -> AST
s "types" asts	= uncurry Types . fmap concat . unzip $ map (\(Type t rs) -> (t,rs)) asts
s "sum" (PrivateT:asts)
		= Priv $ s "sum" asts
s "sum" [Constr name, Types types reqs]
		= UnNamedConstr name types reqs
s "sum" [Constr name, Names names reqs]
		= NamedConstr name names reqs
s _ [ast]	= ast
s nm asts	= seqErr modName nm asts


-- Named-sum-parsing --
-----------------------


modNameN	= modName++"-Named-Sum"

pt2nsum	:: ParseTree -> ([(Maybe Name, Type)], [TypeRequirement])
pt2nsum	=  pt2a hn tn sn convertN . cleanAll ["comma"]

convertN	:: ASTN -> ([(Maybe Name, Type)], [TypeRequirement])
convertN (NamedSum stuff reqs)
		= (stuff, reqs)
convertN ast	= convErr modNameN ast

data ASTN	= NamedSum [(Maybe Name, Type)] [TypeRequirement]
		| IdentN Name
		| Idents [Name]
		| TypeN Type [TypeRequirement]
		| ColonT
	deriving (Show)


hn		:: [(Name, ParseTree -> ASTN)]
hn		=  [("type",uncurry TypeN . pt2type)]

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
sn "optNamed" [TypeN t reqs]
		= NamedSum [(Nothing, t)] reqs
sn _ [Idents ids, TypeN t reqs]
		= NamedSum (zip (map Just ids) $ repeat t) reqs
sn _ asts@(NamedSum _ _:_ )
		= uncurry NamedSum $ concatNamedSum asts
sn _ [ast]	= ast
sn nm asts	= seqErr modNameN nm asts

concatNamedSum	:: [ASTN] -> ([(Maybe Name, Type)],[TypeRequirement])
concatNamedSum []
		= ([],[])
concatNamedSum ((NamedSum a b):tail)
		= let (as, bs) = concatNamedSum tail
			in (a++as,b++bs)
