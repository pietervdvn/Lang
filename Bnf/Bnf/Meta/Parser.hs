module Bnf.Meta.Parser (parse) where

import Control.Monad.Writer
import Data.Map hiding (map)
import Bnf.PtGen (parseFull, lastParsePos)
import Bnf.Meta.BnfRegex
import Bnf.Meta.BnfModule
import Bnf.Meta.BnfExpr

import Bnf.Meta.Pt2Module
import Bnf.Meta.IOModule
import Bnf.Converter 
import Bnf.ParseTree

import Control.Arrow

{- imports all things you need to easily convert a string into an IOModule -}

-- actually parse the string.
parse	:: String -> (Maybe IOModule, [Either Warning Error])
parse str	=  case parseFull world fqnMod "module" str of
			Nothing 		-> (Nothing, [Right (defErrRuleInf $ calcErrPos str, "No parse result")] )
			(Just (Left e))		-> (Nothing, [Right (defErrRuleInf defErrPos, show e)])
			(Just (Right pt))	-> pm pt

pm		:: ParseTree -> (Maybe IOModule, [Either Warning Error])
pm		=  first Just . runWriter . parseModule


defErrRuleInf p	= (fqnMod, "bnf", p)
defErrPos	= (0,0,0)

calcErrPos	:: String -> (Int, Int, Int)
calcErrPos	=  lastParsePos world fqnMod "module"

world	= fromList [(fqnMod, modMod),(fqnBnf, bnfMod),(fqnRegex, regexMod)]
