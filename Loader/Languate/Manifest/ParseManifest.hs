module Languate.Manifest.ParseManifest (parse, parseManifest) where

{--
This module implements the parser
--}

import StdDef
import Languate.Manifest.Manifest
import Exceptions
import qualified Bnf
import Bnf.ParseTree

import Languate.Manifest.Pt2Manifest

import Data.Maybe
import Data.Either

import Control.Arrow

bnfFile	= "bnf/Manifest.bnf"
bnfFQN	= Bnf.toFQN ["Manifest"]

parseManifest	:: Bnf.World -> FilePath -> IO Manifest
parseManifest bnf fp
	= do	manif	<- readFile fp |> parse bnf
		runExceptionsIO' manif

errMsg fp msg
	= "Parse error on manifest "++fp++"\n"

parse	:: Bnf.World -> String -> Exceptions' String Manifest
parse bnf str
	= do	let pt	= Bnf.parseFull bnf bnfFQN "manifest" str
		pt'	<- maybe (halt "The manifest could not be parsed at all") return pt
		either (halt . show) pt2manifest pt' 
