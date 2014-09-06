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

-- info	:: TModule -> Name -> String
info modul n
	= Prelude.map (info' modul) $ signaturesWithName n $ functions modul


info'	:: TModule -> Signature -> String
info' tmod sign
	=  let docstr'	= fromMaybe "-- No docstring found" $ lookupSt sign $ docstrings tmod in
	   let docstr	= if '\n' `elem` docstr' then "--- "++docstr' ++ " ---" else "-- "++docstr' in
	   let loc'	= lookupSt sign $ definedIn tmod in
	   let loc	= (++) "-- Defined in\t" $ show $ fromMaybe (error "No location found. This is a bug") loc' in
	   let imp	= fromMaybe [] $ lookupSt sign $ functions tmod in
		loc++"\n"++docstr++"\n"++show sign++"\n"++showClauses imp


showClauses	:: [Clause] -> String
showClauses []	=  "-- function not representable"
showClauses clauses
		= intercalate "\n" $ fmap show clauses
