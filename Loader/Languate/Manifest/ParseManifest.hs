module Languate.Manifest.ParseManifest (parse, parse') where

{--
This module implements the parser
--}

import StdDef
import Languate.Manifest.Manifest

import qualified Bnf
import Bnf.ParseTree

import Languate.Manifest.Pt2Manifest

import Data.Maybe
import Data.Either

import Control.Arrow

bnfFile	= "bnf/Manifest.bnf"
bnfFQN	= Bnf.toFQN ["Manifest"]

parse'	:: FilePath -> IO Manifest
parse' fp
	= do	bnf	<- Bnf.load bnfFile
		manif	<- readFile fp |> parse bnf
		case manif of
			Left msg	-> do	putStrLn $ errMsg fp msg
						error "Could not parse manifest. See stdout for details"
			Right manifest	-> return manifest

errMsg fp msg
	= "Parse error on manifest "++fp++"\n"

parse	:: Bnf.World -> String -> Either String Manifest
parse bnf str
	= do	let pt	= Bnf.parseFull bnf bnfFQN "manifest" str
		pt'	<- maybe (Left "The manifest could not be parsed at all") Right pt
		case pt' of
			Left msg	-> Left $ show msg
			Right pt	-> Right $ pt2manifest pt
