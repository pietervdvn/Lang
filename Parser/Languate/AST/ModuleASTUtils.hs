module Languate.AST.ModuleASTUtils where

import StdDef

import Languate.AST.TypeAST
import Languate.AST.ModuleAST
import Languate.AST.FunctionAST

import Data.Either

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

isAllowed	:: Restrict -> Name -> Bool
isAllowed (BlackList items)
		= not . (`elem` items)
isAllowed (WhiteList items)
		= (`elem` items)

-- function declarations in module, which are public/private
functions	:: Visible -> Module -> [(Name, Type, [TypeRequirement])]
functions mode mod
		= let 	restrict	= exports mod
			stms	= statements mod in
		  _censor ((mode == Public) ==) restrict $ concatMap _unpackF stms

_censor		:: (Bool -> Bool) -> Restrict -> [(Name, Type, [TypeRequirement])] -> [(Name, Type, [TypeRequirement])]
_censor inv restrict
		= filter (\(nm,_,_) -> inv $ isAllowed restrict nm)



statements	= map fst . statements'


_unpackF	:: Statement -> [(Name,Type, [TypeRequirement])]
_unpackF (FunctionStm f)
		= signs f
_unpackF (ClassDefStm cd)
		= fmap (\(nm,t,tr) -> (nm,t,tr)) $ decls cd
_unpackF _	= []


imports''	:: Module -> [(Import, Coor)]
imports'' 	=  rights . imports

imports'	:: Module -> [Import]
imports'	=  map fst . imports''
