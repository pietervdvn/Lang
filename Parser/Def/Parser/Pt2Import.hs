module Def.Parser.Pt2Import (pt2imp, pt2restrict) where

import StdDef
import Bnf.ParseTree
import Bnf hiding (simpleConvert)
import Control.Monad.Writer
import Control.Monad
import Def.Parser.Utils
import Def.Parser.Pt2Prelude
import Def.Parser.Pt2Type
import Def.Parser.Pt2Expr
import Def.Parser.Pt2Pattern
import Def.Parser.Pt2Comment
import Def.Def

{--

This module converts the ParseTree into a function declaration, with laws etc.
Declarations may have multiple (explicit) types

--}

modName	= "Pt2Import"

pt2imp	:: ParseTree -> Import
pt2imp	=  pt2a h t s convert . cleanAll ["dot"]

convert		:: AST -> Import
convert (Root asts)
		=  buildImport asts
convert ast	=  convErr modName ast


buildImport	:: [AST] -> Import
buildImport []	=  Import False [] "" $ BlackList []
buildImport (PublicT:tail)
		= let Import _ path name restrict = buildImport tail in
			Import True path name restrict
buildImport (ImportT:tail)
		= buildImport tail
buildImport (Path p:tail)
		= let Import public _ _ restrict = buildImport tail in
			Import public (init' p) (last p) restrict
buildImport (Restriction r:tail)
		= let Import public path name _ = buildImport tail in
			Import public path name r
buildImport ast	=  convErr (modName++"-buildImport") $ Root ast

data AST	= ImportT
		| Ident Name
		| Path [Name]
		| Restriction Restrict
		| ParO	| ParC
		| Dot	| PublicT
		| Root [AST]
	deriving (Show)


h		:: [(Name, ParseTree -> AST)]
h		=  [("limiters",Restriction . pt2restrict)]

t		:: Name -> String -> AST
t "globalIdent" id
		= Ident id
t _ "("		= ParO
t _ ")"		= ParC
t "import" "import"
		=  ImportT
t "import" "public"
		=  PublicT
t nm cont	=  tokenErr modName nm cont


s		:: Name -> [AST] -> AST
s _ asts@(Ident _:_)
		= Path $ accPath asts
s _ asts@(Path _:_)
		= Path $ accPath asts
s _ [ast]	= ast
s _ asts	= Root asts

accPath		:: [AST] -> [Name]
accPath []	=  []
accPath (Ident id:tail)
		= id:accPath tail
accPath (Path ids:tail)
		= ids++accPath tail

-- ### Calculate the restrictions

pt2restrict	:: ParseTree -> Restrict
pt2restrict	=  pt2a [] tr sr convertR . cleanAll ["comma"]

data ASTR	= Idnt Name
		| Asts [ASTR]
		| HidingT
		| ShowingT
		| SetO	| SetC
		| ParOR	| ParCR
		| Hide [Name]
		| Show [Name]
	deriving (Show)

convertR	:: ASTR -> Restrict
convertR (Show nms)
		= WhiteList nms
convertR (Hide nms)
		= BlackList nms
convertR ast	=  convErr (modName++"-r") ast

tr		:: Name -> String -> ASTR
tr "localIdent" id
		= Idnt id
tr "globalIdent" id
		= Idnt id
tr "op" id
		= Idnt id
tr _ "{"	= SetO
tr _ "}"	= SetC
tr _ ")"	= ParOR
tr _ "("	= ParCR
tr _ "hiding"	= HidingT
tr _ "showing"	= ShowingT

tr nm cont	=  tokenErr (modName++"-r") nm cont

sr		:: Name -> [ASTR] -> ASTR
sr _ [ast]	= ast
sr _ (HidingT:tail)
		= Hide $ getNames tail
sr _ (ShowingT:tail)
		= Show $ getNames tail
sr _ asts	= Asts asts

getNames	:: [ASTR] -> [Name]
getNames []	=  []
getNames (Idnt name:tail)
		= name:getNames tail
getNames (Asts asts:tail)
		= getNames asts ++ getNames tail
getNames (_:tail)
		=  getNames tail
