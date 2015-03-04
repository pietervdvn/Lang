module Languate.Manifest.Pt2Manifest (pt2manifest) where

import StdDef
import Bnf.ParseTree
import Bnf hiding (simpleConvert)
import Languate.Parser.Utils
import Languate.Manifest.Manifest

import Languate.Manifest.Pt2MetaValue
import Languate.Manifest.CreateManifest

{--
This module converts the ParseTree into a metavalue
--}

modName	= "Pt2Manifest"

pt2manifest	:: ParseTree -> Manifest
pt2manifest	=  pt2a h t s convert . cleanAll ["nl","line","eq"]

convert		:: AST -> Manifest
convert (Man m)	=  m


data AST	= PackName String
		| Synopsis String
		| Descr String
		| MetaV MetaValue
		| FieldName String
		| Fields [(String, MetaValue)]
		| Man Manifest
	deriving (Show)

h		:: [(Name, ParseTree -> AST)]
h		=  [("metaValue", MetaV . pt2metaValue)]

t		:: Name -> String -> AST
t "globalIdent" id
		= PackName id
t "synopsis" s	= Synopsis s
t "description" s
		= Descr s
t "fieldName" s	= FieldName s
t nm cont	=  tokenErr modName nm cont


s		:: Name -> [AST] -> AST
s "field" [FieldName k, MetaV v]
		= Fields [(k, v)]

s "manifest" [PackName name, Synopsis syn, Descr descr, Fields fields]
		= Man $ createFromDict name syn descr fields
s _ [ast]	= ast
s "manifest" (Fields heads:asts)
		= let Fields tails	= s "manifest" asts in
			Fields $ heads ++ tails

s nm asts	=  seqErr modName nm asts
