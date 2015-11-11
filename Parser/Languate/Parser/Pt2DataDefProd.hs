module Languate.Parser.Pt2DataDefProd (pt2prod) where

import StdDef
import Bnf.ParseTree
import Bnf
import Languate.Parser.Utils
import Languate.Parser.Pt2Comment
import Languate.Parser.Pt2DataDefSum
import Languate.Parser.Pt2Type
import Languate.AST

import Data.List (nub)

{--

This module converts the ParseTree into a ADT-def, eg

data ADT = A a b c
	| D e f

It uses pt2datadefsum to do part of the lifting, the heave work is done by pt2prod (product) which adds the comments to it.
--}

modName	= "Pt2DataDefProd"

-- Gives (docstrings with respective constructor name, adt-sums, typeReqs)
pt2prod	:: ParseTree -> ([ADTSum], [TypeRequirement], [DocString Name])
pt2prod =  pt2a h t s convert . cleanAll ["nlt"]

convert		:: AST -> ([ADTSum], [TypeRequirement], [DocString Name])
convert (Constructor vis nm args docs)
		= ([ADTSum nm vis (args ||>> fst)], args |> snd >>= snd, docs |> (`DocString` nm) )
convert (Constructors asts)
		= asts |> convert & unzip3 & (\(ass, bss, css) -> (concat ass, concat bss, concat css))


data AST	= BraceOT	| BraceCT	| CommaT	| SubTypeT
		| Constr	| Comm [String]	| PrivT		| SemiColon
		| Ident Name	| Idents [Name]
		| TypeV (Type,[TypeRequirement])
		| Types [(Maybe Name, (Type, [TypeRequirement]))]
		| Constructor { mode::Visible, nm::Name, tps::[(Maybe Name, (Type, [TypeRequirement]))], docs::[String] }
		| Constructors [AST]
	deriving (Show, Eq)


h		:: [(Name, ParseTree -> AST)]
h		=  [("comment",Comm . (:[]) . pt2comment)
			, ("mlcomment", Comm . (:[]) . pt2comment)
			, ("baseType", TypeV . pt2type)
			, ("type", TypeV . pt2type )
			]

t		:: Name -> String -> AST
t _ ","		=  CommaT
t _ "{"		=  BraceOT
t _ "}"		=  BraceCT
t _ "_"		=  PrivT
t _ ";"		=  SemiColon
t "subTypeT" _	=  SubTypeT
t "localIdent" nm	=  Ident nm
t "constructName" nm	=  Ident nm
t nm cont		=  tokenErr modName nm cont


s		:: Name -> [AST] -> AST
s "prod" (CommaT:rest)
		= s "constructor" rest
s "prod" (BraceOT:conss)
		= s "prod" conss
s "prod" conss	= conss & filter (`notElem` [BraceCT, CommaT]) & Constructors
s "constructor" []
		= Constructor Public (seqErr (modName++": Constructor with no name") "constructor" ([]::[AST])) [] []
s "constructor" (Types tps':rest)
		= let constr = s "constructor" rest in constr {tps = tps' ++ tps constr}
s "constructor" (Comm comm:rest)
		= let constr = s "constructor" rest in constr {docs = comm ++ docs constr}
s "constructor" (Ident name:rest)
		= let constr = s "constructor" rest in constr {nm = name}
s "constructor" (PrivT:rest)
		= let constr = s "constructor" rest in constr {mode = Private}
s "idents" asts	= asts |> (\(Ident nm) -> nm) & Idents
s "named" [Idents nms,SubTypeT,TypeV tp]
		= nms |> (\nm -> (Just nm, tp)) & Types
s "namedSum" []	= Types []
s "namedSum" (SemiColon:rest)
		= s "namedSum" rest
s "namedSum" (Types tps:rest)
		= let Types tail	= s "namedSum" rest in
			Types (tps ++ tail)
s "types" []	= Types []
s "types" (TypeV t:rest)
		= let Types tps	= s "types" rest in
			Types ((Nothing, t):tps)
s _ [ast]	= ast
s nm asts	= seqErr modName nm asts
