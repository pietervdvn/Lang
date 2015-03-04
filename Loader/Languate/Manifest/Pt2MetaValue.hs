module Languate.Manifest.Pt2MetaValue (pt2metaValue) where

import StdDef
import Bnf.ParseTree
import Bnf hiding (simpleConvert)
import Languate.Parser.Utils

import Languate.Manifest.Manifest

{--

This module converts the ParseTree into a metavalue

--}

modName	= "Pt2MetaValue"

pt2metaValue	:: ParseTree -> MetaValue
pt2metaValue	=  const (Int 42)-- pt2a h t s convert . cleanAll ["dot","comma","line"]

convert		:: AST -> MetaValue
convert ast	=  todos $ show ast


data AST	= AInt Int
	deriving (Show)

h		:: [(Name, ParseTree -> AST)]
h		=  []

t		:: Name -> String -> AST
t nm cont	=  tokenErr modName nm cont


s		:: Name -> [AST] -> AST
s nm asts	=  seqErr modName nm asts
