module Bnf.Meta.BnfModule where
	
{- The syntactic rules of a bnf- module -}

import Bnf.BNF
import Bnf.Meta.BnfExpr (fqnBnf, rgx, localIdent)
import Bnf.Meta.BnfRegex (fqnRegex)
import Bnf.FQN
import Data.Map (fromList)

fqnMod	= FQN [] "module"
modMod	= Module (fromList 
		[("module", modul),("imports", imports),
			("import", imprt),("moduleName", moduleName),("hider", hider),("globalIdent", globalIdent),("packName", packName),
			("meta",metas),("metafield",field),("content", content),("string",string),("int",int),("list",list),("listContent", listContent),
			("localIdent", localIdent),("rules",rules),("rule",rule),
			("modifier",modifier),("nl",nl),("dquote",rgx "\""),("comma",rgx ",")]) 
		(fromList [("expression", fqnBnf),("comment",fqnBnf),("regex", fqnRegex)])



modul	= Seq [Call "moduleName",Call "nl", Call "meta", Call "imports", Call "rules"]

-- rules

rules		= More $ Seq [Call "rule", Call "nl"]
rule		= Seq [ Call "modifier", Call "localIdent", rgx "=|::=", Call "expression",Opt $ Call "comment"]
modifier	= Star $ rgx "[>_$]"
nl		= More $ Seq [Opt $ Call "comment", rgx "\n"]


--- Metadata

metas		= More $ Seq [Call "metafield", Call "nl"]
field		= Seq [Call "packName", Opt $ rgx ":|=", Call "content"]
content		= Choice [Call "string", Call "int", Call "list"]
string		= Seq [Call "dquote", rgx "(![\"]|\\\\\")*", Call "dquote"]
int		= rgx "[0..9]+"
list		= Seq [rgx "\\[", Call "listContent", rgx "\\]"]
listContent	= Star $ Seq [Choice [Call "string", Call "int"], Opt $ Call "comma"]

--- Imports

-- the [a..z]* after import is to make sure people use whitespace (localIdent will fail)
imports		= Star $ Seq [Call "import", Call "nl"]
imprt		= Seq [ Opt $ rgx "public", rgx "import[a..z]*", Call "moduleName", Opt $ Call "hider"]
moduleName	= Choice [ Seq [ Call "packName", rgx "\\.", Call "moduleName"],  Call "globalIdent" ]
hider		= Seq [ Choice [ rgx "hiding", rgx "showing"], rgx "\\{", More $ Seq [Call "localIdent", Opt $ Call "comma"], rgx "\\}" ]
globalIdent	= rgx "[A..Z][a..zA..Z0..9]*"
packName	= rgx "[a..z][a..zA..Z0..9]*"
{-


import		::= ows public? ows imp ows module (listImport | hider) nls

_public		::= "public"
_imp		::= "import"

_module		::= (localIdent dot)* globalIdent

_showing	::= "showing"
_hiding		::= "hiding"

_hider		::= (showing | hiding) ows  "\[" ows localIdent (listSep localIdent)* ows"\]" ows
_listImport	::= (listSep module)* ows



-}
