module Languate.PackageTable where

import StdDef
import Languate.MarkUp as Mu


import Languate.FQN
import Languate.AST
import Languate.Package
import Languate.ModuleTable
import Languate.PrecedenceTable
import Languate.Typetable.TypeLookupTable

import Languate.CheckUtils

import Data.Map


data PackageTable
	= PackageTable {
		moduleTables	:: Map FQN ModuleTable,
		precedenceTable	:: PrecedenceTable
		}
	deriving (Show)


buildPackageTable	:: Package -> Exc PackageTable
buildPackageTable p
	= do	precT		<- buildPrecTable p
		tlts		<- buildTLTs p
		modTbls		<- modules p & toList |> _buildModuleTable (buildModuleTable p precT tlts) & sequence |> fromList
		return $ PackageTable modTbls precT

_buildModuleTable	:: (FQN -> Module -> Exc ModuleTable) -> (FQN, Module) -> Exc (FQN, ModuleTable)
_buildModuleTable f (fqn, mod)
	= do	modT	<- f fqn mod
		return (fqn, modT)



instance Documentable PackageTable where
	toDocument	= _td


_td	:: PackageTable -> (Doc, [Doc])
_td pt	=  let neededDocs	= addDocs' (precedenceTable pt) $ moduleTables pt & toList >>= mod2doc in
			(doc "Package table" "" $ neededDocs |> title |> inlink & Mu.List, neededDocs)
