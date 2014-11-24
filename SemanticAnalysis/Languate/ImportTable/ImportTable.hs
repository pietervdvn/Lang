module Languate.ImportTable.ImportTable where

{--

This module implements an import table, or simply which modules are visible by what names

--}

import StdDef
import Data.Map hiding (foldl, map)
import Data.Either
import Languate.AST
import Languate.FQN
import qualified Data.Set as S
import Data.Set (Set)

-- aliasses are indistinguisable here, all are treated as equals. Sometimes multiple imports are possible, then the lookup should be more qualified.
type ImportTable	= Map ([Name], Name) (Set FQN)

-- we assume that all imports were resolved and correct
buildImportTable	:: Module -> (Import -> FQN) -> ImportTable
buildImportTable mod i2fqn
			=  let 	imps	= imports' mod
				fqns	= zip imps (map i2fqn imps) in
				foldl (flip addToTable) empty fqns

addToTable	:: (Import, FQN) -> ImportTable -> ImportTable
addToTable (Import _ _ _ (Just pseudo) _, fqn) it
		=  insertSet ([],pseudo) fqn it
addToTable (Import _ names name _ _, fqn) it
		= let	kys	= [(postfix, name) | postfix <- tails names] in
			foldl (\it k -> insertSet k fqn it) it kys


insertSet	:: (Ord k, Ord v) => k -> v -> Map k (Set v) -> Map k (Set v)
insertSet k v dict
		=  insert k (S.insert v (lookupSet k dict)) dict

lookupSet	:: Ord k => k -> Map k (Set v) -> Set v
lookupSet 	=  findWithDefault (S.empty)
