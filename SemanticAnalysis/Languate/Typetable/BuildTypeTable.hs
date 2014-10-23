module Languate.Typechecker.BuildSuperTypeTable where

{--
This module provides functions to build the supertype table
--}

buildSuperTypeTable	:: [Statement] -> SuperTypeTable






typeDecls	:: [Statement]


-- tells you wether or not a statement declares a new type, (by introducing a new class, adt or subtype)
isTypedecl			:: Statement -> Bool
isTypedecl (ADTDefStm _)	= True
isTypedecl (SynDefStm _)	= True
isTypedecl (SubDefStm _)	= True
isTypedecl (ClassDefStm _)	= True
