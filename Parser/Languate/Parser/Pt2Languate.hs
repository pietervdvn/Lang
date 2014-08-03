module Languate.Parser.Pt2Languate (pt2mod) where

import StdDef
import Bnf.ParseTree
import Bnf
import Languate.Parser.Utils
import Languate.Parser.Pt2Import
import Languate.Parser.Pt2Comment
import Languate.Parser.Pt2Statement
import Languate.Parser.Pt2Law
import Languate.AST

{--

Where it all comes together.


This module converts the ParseTree into a full module.

--}

modName	= "Pt2Lang"

pt2mod	:: ParseTree -> Module
pt2mod	=  pt2a h t s convert

convert		:: AST -> Module
convert ast
		= toMod ast (Module "" (BlackList []) [] [])


toMod		:: AST -> Module -> Module
toMod (Ident name)
		= setname name
toMod (Exports exps)
		= setExports exps
toMod Nl	= id
toMod (Comms c)	= addStm $ Comments c
toMod (Imps comms imp)
		= modImports $ (++) $ map Left comms ++ [Right imp]
toMod (Imp imp)
		= modImports (Right imp:)
toMod (Stm stms)
		=  addStms stms
toMod (Root asts)
		=  \mod -> foldr toMod mod asts
toMod (Examp ex)
		=  addStm $ ExampleStm ex



data AST	= Ident Name
		| Exports [Name]
		| Comms [Comment]
		| Imp Import
		| Imps [Comment] Import
		| Stm [Statement]
		| Examp Law
		| Root [AST]
		| Nl
	deriving (Show)


h		:: [(Name, ParseTree -> AST)]
h		=  [("idSet", Exports . pt2idset),("nlcomment", Comms . (:[]) . pt2comment)
		   , ("import", Imp . pt2imp), ("example", Examp . pt2law)
		   , ("nls", Comms . pt2nls),("statement", Stm . pt2stm)]

t		:: Name -> String -> AST
t "globalIdent" id
		= Ident id
t _ "\n"	=  Nl
t nm cont	=  tokenErr modName nm cont


s		:: Name -> [AST] -> AST
s _ [Comms coms, Imp imp]
		= Imps coms imp 
s _ [ast]	= ast
s _ asts	= Root asts

