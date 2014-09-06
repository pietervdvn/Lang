module Languate.TypedPackage where

-- A typedPackage contains all the info needed for the interpreter. It is the endgoal of the semantic analysis


import Languate.TAST
import Languate.AST
import Languate.SymbolTable
import Languate.FQN
import Data.Map as M
import Normalizable


type TypedPackage	= TPackage
type TPackage	= Map FQN TModule


data TModule	= TModule {typedClauses:: SymbolTable [TClause], docstrings::SymbolTable DocString, functions::SymbolTable [Clause], definedIn::SymbolTable FQN}
	deriving (Show)


normalizePackage	:: TPackage -> TPackage
normalizePackage	= M.map normalize

instance Normalizable TModule where
	normalize (TModule tc docs funcs defin)
		= TModule (normalizeSigns tc) (normalizeSigns docs) (normalizeSigns funcs) (normalizeSigns defin)
