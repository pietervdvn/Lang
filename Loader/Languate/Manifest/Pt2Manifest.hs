module Languate.Manifest.Pt2Manifest (pt2manifest) where

import StdDef
import Bnf.ParseTree
import Bnf hiding (simpleConvert)
import Languate.Parser.Utils
import Languate.Manifest.Manifest

import Languate.Manifest.Pt2MetaValue

{--

This module converts the ParseTree into a metavalue

--}

modName	= "Pt2Manifest"

pt2manifest	:: ParseTree -> Manifest
pt2manifest	=  pt2a h t s convert

convert		:: AST -> Manifest
convert ast	=  todos $ show ast


data AST	= MetaV MetaValue
	deriving (Show)

h		:: [(Name, ParseTree -> AST)]
h		=  [("metaValue", MetaV . pt2metaValue)]

t		:: Name -> String -> AST
t nm cont	=  tokenErr modName nm cont


s		:: Name -> [AST] -> AST
s nm asts	=  seqErr modName nm asts
