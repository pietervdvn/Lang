module Languate.Manifest2Doc where

import StdDef
import Languate.MarkUp
import Data.Char

import Languate.Manifest as Mani

import Control.Arrow
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe

manifestDocTitle	= "Manifest overview"

manifest2doc	:: Manifest -> Doc
manifest2doc manif
		= doc manifestDocTitle "All values of the manifest" $
			table ["Property","Value"] $ _manif2table manif ++ _manif2tableRest manif



_manif2table manif	= [("name",String . Mani.name)
				,("synopsis", String . synopsis)
				,("description", String . Mani.description)
				,("version",Version . version)
				,("language",Version . language)
				,("authors",Lst . map String . authors)
				,("depends", Dict . (|> mapTuple (String . show , Version)) . M.toList . depends)
				,("prelude", St . (|> ModuleName) . S.toList . prelude)
				,("exposes",St . (|> ModuleName) . S.toList . exposes)
				,("execute", String . fromMaybe "" . execute) ]
			|> second (\f -> f manif & show)
			||>> uppercaseWord
			|> mapTuple (Base, code) |> asList

_manif2tableRest manif
	= manif & rest & M.toList |> mapTuple (Base . uppercaseWord, code . show) |> asList

uppercaseWord	:: String -> String
uppercaseWord (w:ord)	= toUpper w:ord
uppercaseWord ""	= ""

asList (a,b)	= [a,b]
