module Languate.AST.ModuleASTUtils where

import StdDef
import Languate.AST.ModuleAST

setname	:: Name -> Module -> Module
setname name (Module _ restrict imps stms)
		= Module name restrict imps stms

setExports	:: [Name] -> Module -> Module
setExports names (Module name _ imps stms)
		= Module name (WhiteList names) imps stms

modImports		:: (Imports -> Imports) -> Module -> Module
modImports f (Module name restrict imps stms)
		= Module name restrict (f imps) stms

addStm		:: Coor -> Statement -> Module -> Module
addStm _ (Comments []) mod
		= mod
addStm coor stm (Module name restrict imps stms)
		= Module name restrict imps ((stm,coor):stms)


addStms		:: [(Statement,Coor)] -> Module -> Module
addStms stms mod
		= foldr (uncurry $ flip $ addStm) mod stms
