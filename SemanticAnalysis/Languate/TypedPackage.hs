module Languate.TypedPackage where

-- A typedPackage contains all the info needed for the interpreter. It is the endgoal of the semantic analysis


import Languate.TAST
import Languate.AST
import Languate.SymbolTable
import Data.Map

type TypedPackage	= TPackage
type TPackage	= Map FQN TModule


data TModule	= TModule {typedClauses:: Symboltable [TClause], docstrings::SymbolTable DocString, functions::SymbolTable [Clause], definedIn::SymbolTable [FQN]}
