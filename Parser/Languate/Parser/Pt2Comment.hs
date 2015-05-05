module Languate.Parser.Pt2Comment (pt2comment, pt2nl, pt2nls,pt2nlcomments) where


import StdDef
import Bnf.ParseTree
import Bnf hiding (simpleConvert)
import Data.Maybe

import Languate.AST hiding (Tuple)
import Languate.Parser.Utils
{--
See module name
--}

data AST	= Comment String
		| Comms [String]
		| CommentO	| MlCommDelim
		| Nl
	deriving (Show)

t		:: Name -> String -> AST
t "comment" "--"= CommentO
t _ "\n"	= Nl
t _ "\n\t"	= Nl
t _ text	= if all ('-'==) text then MlCommDelim
			else Comment text


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
		= Comment $ str
s _ [MlCommDelim, Nl]
		= Comment ""	-- emtpy comment detected!
s _ [ast]  	= ast
s nm ast	= seqErr "Pt2Comment" nm ast


-- can be used for comment, mlcomment, nlcomment
pt2comment	:: ParseTree -> Comment
pt2comment	=  pt2a [] t s (\(Comment str) -> str)

sNl		:: Name -> [AST] -> AST
sNl _ [Comment str, Nl]
		= Comment str
sNl n asts	= s n asts

-- used to parse "nl" and "nltab", which might contain a comment
pt2nl		:: ParseTree -> Maybe Comment
pt2nl		=  pt2a hNl t sNl (\ ast ->
			case ast of
				(Comment str)	-> Just str
				(Nl)		-> Nothing)

hNl		:: [(Name, ParseTree -> AST)]
hNl		= [("comment",pt2ast),("nlcomment", pt2ast)]
			where pt2ast	= simpleConvert [] t s


pt2nlcomments	=  pt2nls
pt2nls		:: ParseTree -> [Comment]
pt2nls		=  pt2a hNls (tokenErr "Pt2Comment-nls") sNls (\(Comms strs) -> strs)


hNls		:: [(Name, ParseTree -> AST)]
hNls		=  [("nl", Comms . catMaybes . (:[]) . pt2nl),("nlcomment",Comment . pt2comment)]

sNls		:: Name -> [AST] -> AST
sNls "nlcomments" asts
		= Comms $ map (\(Comment c) -> c) asts
sNls "nls" [Nl]	= Comms []
sNls "nls" asts = Comms $ concatMap (\(Comms strs) -> strs) asts
