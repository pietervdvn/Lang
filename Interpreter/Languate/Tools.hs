module Languate.Tools where

{--

This module implements some functions to help people with the interpreter.
It contains

> info "functionName"
which will all it knows (docstring, type, laws, implementation, location)
TODO add laws to info

--}

import StdDef
import Languate.TypedPackage
import Languate.FQN
import Languate.SymbolTable
import Languate.AST
import Languate.Signature
import Data.Map as Map
import Data.Maybe
import Data.List

info	:: TModule -> Name -> String
info modul n
	= let signatures =  signaturesWithName n $ functions modul in
	  let merged		= _merge (zip signatures $ Prelude.map (info' modul) signatures) in
	  let merged'		= "\n-- ##\t"++n++"\n\n"++ merged in
	  let unmerged		= info' modul $ head signatures in
	  let notFound		= "-- No function with name "++n++" found" in
	  let nr		= length signatures in
	  case nr of
		0	-> notFound
		1	-> unmerged
		_	-> merged'
	  
		

_merge		::  [(Signature, String)] -> String
_merge docs
		= concat $ Data.List.map (\(Signature _ t, str) -> "-- ###\t" ++ show t ++ "\n" ++ str) docs

info'	:: TModule -> Signature -> String
info' tmod sign
	=  let found	= isJust $ lookupSt sign $ functions tmod in
	   let docstr'	= fromMaybe "-- No docstring found" $ lookupSt sign $ docstrings tmod in
	   let docstr	= if '\n' `elem` docstr' then "--- "++docstr' ++ " ---" else "-- "++docstr' in
	   let loc'	= fromMaybe (error $ "No location found. This is a bug (interpreter/Tools/info)"++show sign) $ lookupSt sign $ definedIn tmod :: FQN in
	   let loc	= "-- Defined in\t[" ++ (show loc') ++ "]" in
	   let imp	= fromMaybe [] $ lookupSt sign $ functions tmod in
	   let result	= loc++"\n"++docstr++"\n"++show sign++"\n"++showClauses imp in
	   if found then result else "-- No function found with signature "++show sign


showClauses	:: [Clause] -> String
showClauses []	=  "-- function not representable"
showClauses clauses
		= intercalate "\n" $ fmap show clauses
