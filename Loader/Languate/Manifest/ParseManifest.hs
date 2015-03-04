module Languate.Manifest.ParseManifest (parse, parse') where

{--
This module implements the parser
--}

import StdDef
import Languate.Manifest.Manifest

import qualified Bnf

import Data.Maybe
import Data.Either

bnfFile	= "bnf/Manifest.bnf"
bnfFQN	= Bnf.toFQN ["Manifest"]

parse'	:: FilePath -> IO Manifest
parse' fp
	= do	bnf	<- Bnf.load bnfFile
		readFile fp |> parse bnf

parse	:: Bnf.World -> String -> Manifest
parse bnf str
	= let	pt	= Bnf.parseFull bnf bnfFQN "manifest" str
		pt'	= fromMaybe (error $ "The manifest could not be parsed") pt
		pt''	= either (error . (++) "Error: no parse result: " . show) id pt'  in
		todos $ show pt''
