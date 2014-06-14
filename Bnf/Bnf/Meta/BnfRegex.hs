module Bnf.Meta.BnfRegex where

import Bnf.BNF
import Bnf.PtGen
import Data.Map (empty, fromList)
import Data.List (intersperse)
import Regex
import Bnf.FQN



fqnRegex	= FQN [] "Regex"
regexMod	= Module (fromList [
			("range", range),("rangeUnit", rangeUnit), ("rangeUnitChar",rangeUnitChar),
			("dot",dot),("metaChar",metaChar),("normalChar",normalChar),("not",nt),
			("pass1", pass1),("pass2",pass2),("int", int),("times", times),
			("postfix", postfix),("pass3",pass3),("sequence",sequenceRule),
			("regex",regexRule),("or", rgx "\\|"), ("and", rgx "\\&")] ) empty

{--
This module implements the syntactic rules for a regex
--}

regexRule	= Choice [ Seq [Call "sequence", Choice [Call "or", Call "and"], Call "sequence"] , Call "sequence" ]
sequenceRule	= Star $ Call "pass3"
		-- pass 3 appends a postfix operator to the pass2 regex.
		-- the operators are: postfix: '*?+' , times: '{i,j}' '{i}'
pass3		= Seq [Call "pass2", Opt $ Choice $ map Call ["postfix", "times"]]
postfix		= rgx "[+?*]"
times		= Choice [Seq [ rgx "\\{", _oInt, rgx ",", _oInt, rgx "\\}"] , Seq [rgx "\\{", _int, rgx "\\}"]]
int		= rgx "[0..9]+" 
_int		= Call "int"
_oInt		= Opt _int

pass2		= Seq [ Opt $ Call "not", Call "pass1" ]

nt		= rgx "\\!"
--_pass1		::= normalChar | metachar| range | "\(" regex "\)"
pass1		= Choice $ Seq [rgx "\\(", Call "regex", rgx "\\)"] : map Call ["normalChar","metaChar", "range"]

normalChar	= Choice [ rgx $ "!["++__meta++"]" , rgx $ "\\\\["++__meta++"]"]
__meta		= "+*?!(|&){}\"" ++ concatMap __literal "\\.[]"
metaChar	= rgx "\\\\n|\\\\t|\\\\r"

-- _range		::= "\[" rangeUnit*  "\]"
-- _rangeUnit	::= rangeUnitChar dot dot rangeUnitChar | rangeUnitChar
-- _rangeUnitChar	::=  "\\[tnf]" | "\\[\[\]\.\"\\]" | "![\[\.\]\"]"

range		= Seq [ rgx "\\[" , More (Call "rangeUnit"), rgx "\\]"]
rangeUnit	= Choice [ Seq [Call "rangeUnitChar", _dot, _dot, Call "rangeUnitChar"] , Call "rangeUnitChar"]
rangeUnitChar	= Choice [  rgx "\\\\[tnf]" , rgx $ "\\\\" ++ __escapedChars, rgx $ '!':__escapedChars]


__escapedChars	= "[" ++ "\"" ++ concatMap __literal "[]\\." ++ "]"
__literal c	= '\\':[c]

-- dot	::= "\."
_dot		= Call "dot"
rgx 		= NWs . Rgx . regex
dot		= rgx "\\."
