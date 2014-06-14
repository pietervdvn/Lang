module Bnf.Meta.BnfExpr where

import Bnf.BNF
import Bnf.Meta.BnfRegex (fqnRegex)
import Data.Map (fromList)
import Regex
import Bnf.FQN

fqnBnf	= FQN [] "bnf"
bnfMod	= Module (fromList [("expression", expression),("sequence", sequenceRule),("factor", factor),("term", term),("localIdent", localIdent),("or",bar),("dquote",dquote),("comment", comment)]) (fromList [("regex", fqnRegex)])



{--

This module implements the rules to parse a Bnf. These can (and will) be used to parse bnf files.
--}


comment		= Choice [ Seq [rgx "--", rgx "!\n*"] 
			, Seq [ rgx "\\{-" , Star $ Choice 
				[More $ rgx "!-", rgx "-+![-}]"] , rgx "-+\\}"] ]

--  "/\*" ("!\*"+ | "\*+![*/]" )* "\*+/"

expression	= Choice [ More $ Seq [Call "sequence", _or, Call "expression"] , Call "sequence" ]

sequenceRule	= More $ Call "factor"

factor		= Seq [ Call "term", Opt $ rgx "[+*?]"]

term		= Choice [ Call "localIdent"
			 , Seq [_dquote, Call "regex", _dquote]
			 , Seq [rgx "\\(", Call "expression", rgx "\\)"]
			 , Seq [ rgx "\\{", Call "sequence", More $ Seq [_or, Call "sequence"], rgx "\\}"] ] 

localIdent	= rgx "[a..z][a..zA..Z0..9]*'?"

_or		= Call "or"
bar		= Seq [Opt $ Call "comment", rgx "(\n\t)?", rgx "\\|"]
dquote		= rgx "\""
_dquote		= Call "dquote"
rgx 		= Rgx . regex
wsrgx		= NWs . rgx	
