module Languate.Typechecker.BuildTypeTable where

{--
This module provides functions to build the type table
--}

buildTypeTable	:: [Statement] -> TypeTable

btt		:: Statement -> State TypeTable ()
btt (ADTDefStm )
