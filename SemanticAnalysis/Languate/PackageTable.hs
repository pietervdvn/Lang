module Languate.PackageTable where

import StdDef
import Languate.MarkUp as Mu


import Languate.FQN
import Languate.AST
import Languate.Package
import Languate.ModuleTable
import Languate.PrecedenceTable
import Languate.Typetable.TypeLookupTable
import Languate.Typetable.BuildTypetable

import Languate.CheckUtils

import Data.Map as M
import Control.Arrow


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
		tts		<- buildTypetables p tlts
		modTbls		<- modules p & M.keys |> (id &&& id) |+> onSecond (assembleModTable tlts tts)
					|> M.fromList
		return $ PackageTable modTbls precT




instance Documentable PackageTable where
	toDocument	= _td


_td	:: PackageTable -> (Doc, [Doc])
_td pt	=  let neededDocs	= addDocs' (precedenceTable pt) $ moduleTables pt & toList >>= mod2doc in
			(doc "Package table" "" $ neededDocs |> title |> inlink & Mu.List, neededDocs)
