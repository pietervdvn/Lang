module Def.Pt2Comment (pt2comment, pt2nl, pt2nls) where


import StdDef
import Bnf.ParseTree
import Bnf
import Bnf.Converter hiding (convert)
import Control.Monad.Writer
import Data.Maybe

import Def.Def hiding (Tuple)
{--
See module name
--}

data AST	= Comment String
		| Comments [String]
		| CommentO	| MlCommDelim
		| Nl
	deriving (Show)

t		:: Name -> String -> AST
t "comment" "--"= CommentO
t _ "---"	= MlCommDelim
t _ "\n"	= Nl
t _ "\n\t"	= Nl
t _ text	= Comment text


s		:: Name -> [AST] -> AST
s "comment" [CommentO, str]
		= str
s "mlcomment" [MlCommDelim, str, MlCommDelim]
		= str
s "nlcomment" (Nl:terms)
		= s "nlcomment" terms
s "nlcomment" []
		= Nl
s "nlcomment" (Comment str:_)
		= Comment str
s _ [ast]  	= ast
s nm ast	= error $ "Pt2Type: Sequence fallthrough for rule '"++nm++"' with content "++show ast


pt2ast	:: ParseTree -> Writer Errors AST
pt2ast	= simpleConvert (const2 Nothing) t s

-- can be used for comment, mlcomment, nlcomment
pt2comment	:: ParseTree -> Writer Errors Comment
pt2comment pt	=  do	(Comment str)	<- pt2ast pt
			return str

sNl		:: Name -> [AST] -> AST
sNl _ [Comment str, Nl]
		= Comment str
sNl n asts	= s n asts

-- used to parse "nl" and "nltab", which might contain a comment
pt2nl		:: ParseTree -> Writer Errors (Maybe Comment)
pt2nl pt	=  do	ast	<- simpleConvert hNl t sNl pt
			return $ case ast of
					(Comment str)	-> Just str
					(Nl)		-> Nothing

hNl		:: Name -> ParseTree -> Maybe (Writer Errors AST)
hNl "comment"	=  Just . pt2ast
hNl "nlcomment"	=  Just . pt2ast
hNl _		= const Nothing



pt2nls		:: ParseTree -> Writer Errors [Comment]
pt2nls pt	=  do	Comments strs	<- simpleConvert hNls (error "Pt2Comment PANIC") sNls pt
			return strs

hNls		:: Name -> ParseTree -> Maybe (Writer Errors AST)
hNls "nl" pt	=  Just $ do	comm	<- pt2nl pt
				return $ Comments $ catMaybes [comm]
hNls _ _	=  Nothing

sNls		:: Name -> [AST] -> AST
sNls "nls" [Nl]	= Comments []
sNls "nls" asts = Comments $ concatMap (\(Comments strs) -> strs) asts
