module Languate.Manifest.Pt2MetaValue (pt2metaValue) where

import StdDef
import Bnf.ParseTree
import Bnf hiding (simpleConvert)
import Languate.Parser.Utils

import Languate.Manifest.Manifest

import Languate.Parser.Pt2Prelude

import Debug.Trace

{--

This module converts the ParseTree into a metavalue

--}

modName	= "Pt2MetaValue"

pt2metaValue	:: ParseTree -> MetaValue
pt2metaValue	=  pt2a h t s id . cleanStrs ["{","}","[","]","-->"] .
					cleanAll ["dot","comma","line"]

h	:: [(Name, ParseTree -> MetaValue)]
h	=  	[ ("string", String . parseString)
		, ("nat", Int . parseNat)
		]

t		:: Name -> String -> MetaValue
t "localIdent" s
		= FuncName s
t "globalIdent" s
		= GlobId s
t nm cont	=  tokenErr modName nm cont


s		:: Name -> [MetaValue] -> MetaValue
s "version" [Int i]
		= Version [i]
s "version" (Int i:asts)
		= let (Version is)	= s "version" asts in
			Version (i:is)
s "version" (Version i:asts)
		= let (Version is)	= s "version" asts in
			Version (i++is)
s "version" [Int i, Version is]
		= Version $ i:is
s "dictValue" [k,v]
		= Dict [(k,v)]
s "license" [GlobId name, String file]
		= License $ File name file
s "moduleName" [GlobId id]
		= ModuleName [id]
s "commaSepValue" [Lst vs]
		= s "commaSepValue" vs
s "commaSepValue" [v]
		= Lst [v]
s "set" [Lst vs]
		= St vs
s _ [ast]	= ast
s "moduleName" (ModuleName id:tail)
		= let ModuleName t	= s "moduleName" tail in
			ModuleName $ id ++ t
s "commaSepValue" (Lst vs:tail)
		= let Lst vs'	= s "commaSepValue" tail in
			Lst $ vs++vs'
s "commaSepValue" (v:tail)
		= let Lst vs'	= s "commaSepValue" tail in
			Lst $ v:vs'
s nm asts	=  seqErr modName nm asts
