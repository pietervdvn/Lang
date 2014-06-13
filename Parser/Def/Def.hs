module Parser.Def.Def where

{--

This module implements the data structures representing a parsed module.

The data flow is:

String -> parsetree -> ast (per bnf-module) -> here defined data structures -> Cleaned up ADT's/CORE (typechecked/type-inferred/disambiguated functions) -> Interpreter

The structures here include comments (except those withing expressions); the data structures preserve order. These data structures are thus a good starting point for doc generation.

--}







