module Languate.Parser.Utils where

-- some convenience methods which return often

import Bnf hiding (simpleConvert)
import Bnf.ParseTree
import qualified Bnf.Converter as Conv
import StdDef
import Languate.AST

import Control.Monad.Writer


-- does all the work! Hooks, tokenize, sequencer, convertor
pt2a		:: [(Name, ParseTree -> ast)] -> (Name -> String -> ast) -> (Name -> [ast] -> ast) -> (ast -> a) -> ParseTree -> a
pt2a h t s conv	=  conv . simpleConvert h t s

-- just like the real simple convert, but with this exception-writer things stripped out.
simpleConvert	:: [(Name, ParseTree -> ast)] -> (Name -> String -> ast) -> (Name -> [ast] -> ast) -> ParseTree -> ast
simpleConvert h t s pt
	=  fst $ runWriter $ Conv.simpleConvert (_modify h) t s pt


-- used to convert the simple hooks into a form which writes errors
_modify		:: (Monad m) => [(Name, ParseTree -> ast)] -> Name -> ParseTree -> Maybe (m ast)
_modify hs nm pt=  case lookup nm hs of
			Nothing	-> Nothing
			(Just f')	-> Just $ return $ f' pt


-- only tokenizing and ast unpacking convertor. Used in pt2prelude
exec		:: (Name -> String -> ast) -> ParseTree -> ast
exec t		=  simpleConvert [] t (\_ [f] -> f)


convErr		:: Show ast => Name -> ast -> a
convErr str ast	=  error $ "Convert error in "++str++" on "++show ast

tokenErr str nm tok
		=  error $ "Tokenize error in "++str++" on rule "++nm++" with token "++show tok

seqErr str nm asts
		=  error $ "Sequencer error in "++str++" on rule "++nm++" with asts "++show asts
