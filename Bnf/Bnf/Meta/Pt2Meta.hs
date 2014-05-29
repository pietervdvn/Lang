module Bnf.Meta.Pt2Meta where

{--
This module implements the conversion from PT to meta info
--}
import StdDef
import Bnf.ParseTree
import Bnf.Converter
import Bnf.Meta.IOModule
import Control.Monad.Writer

parseMeta	:: ParseTree -> Writer Errors IOMeta
parseMeta pt	= do 	conved	<- simpleConvert h t s pt
			return $ conv (getPosition pt) conved

data AST	= Field Name AST
		| FieldName Name
		| List [AST]
		| Str String	| Int Int
		| Sep	| DQuote
		| ListO | ListC
	deriving (Show)

conv	:: Position -> AST -> IOMeta
conv pos (Field name (List asts))
	= (name, Right $ map convData asts, pos)
conv pos (Field name cont)
	= (name,Left $ convData cont, pos)

convData	:: AST -> Either Int String
convData (Str string)
		= Right string
convData (Int int)
		= Left int


h	:: Name -> ParseTree -> Maybe a
h _ _	=  Nothing

t	:: Name -> String -> AST
t "packName" name	= FieldName name

t "int" str		= Int $ read str
t "string" str		= Str str
t "metafield" "="	= Sep
t "metafield" ":"	= Sep
t _ "\""		= DQuote
t _ "["			= ListO
t _ "]"			= ListC
t _ ","			= Sep

t nm str	= error $ "[Pt2Meta] Tkn fallthrough on "++nm ++" with "++show str

s	:: Name -> [AST] -> AST
s "content" [cont]	= cont
s "string" [DQuote, str, DQuote]
		= str
s "list" [ListO, cont, ListC]
		= cont

s "listContent" [item, Sep]
		= item
s "listContent" [item]
		= item
s "listContent" items
		= List items
		
s "metafield" (FieldName name:Sep:rest)
		= s "metafield" $ FieldName name:rest
s "metafield" [FieldName name, content]
		= Field name content

s _ [ast]	= ast
s nm items	=  error $ "[Pt2Meta] Sq fallthrought on "++nm++" with "++show items
