module Test where

import Bnf
import Data.Maybe
import Normalizable
import Data.Maybe
import Bnf.ParseTree

import Def.Pt2Type
import Def.Pt2Expr
import Control.Monad.Writer

import System.IO.Unsafe
{--

This module loads and compiles the bnf's to test them 

--}

pt		:: String -> String -> IO ParseTree
pt rule str	=  do	world	<- load "bnf/Languate"
			let pt'	= fromJust $ parse world (toFQN ["Languate"]) rule $ str++"\n"
			let pt  = case pt' of
					Right pt	-> pt
					Left exception	-> error $ show exception
			return pt

-- ts rule str	=  pt rule str >>= print . simplify

-- tr rule str	= pt rule str >>= print

tf		:: FilePath -> IO ()
tf fp		=  do	str 	<- readFile fp
			pt "lang" str >>= print . simplify

ts str	= fst $ runWriter $ pt2expr (unsafePerformIO $ pt "expr" str)
