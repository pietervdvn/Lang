module Languate.TypeTable.BuildKnownTypes where

import StdDef
import qualified Exceptions as E
import Exceptions hiding (err)
import Languate.CheckUtils

import Languate.Package
import Languate.FQN
import Languate.TypeTable
import Languate.AST
import Languate.TAST

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Set hiding (map, filter)
import Data.Map hiding (map, filter, mapMaybe)
import Data.Maybe
import Data.Tuple
import Control.Arrow (first, second)
import Data.List

buildKnownTypes	:: Package -> Set TypeID
buildKnownTypes package
		= package & modules & M.toList
			|> (\(fqn,mod) -> zip (repeat fqn) $ locallyDeclared package fqn)
			& concat & S.fromList



-- All locally declared types, as a set
locallyDeclared	:: Package -> FQN -> [Name]
locallyDeclared	w fqn
		=  mapMaybe declaredType $ statements $ fwd "modules" fqn $ modules w


declaredType	:: Statement -> Maybe Name
declaredType (ADTDefStm (ADTDef name _ _ _))
		= Just name
declaredType (SynDefStm (SynDef name _ _ _))
		= Just name
declaredType (SubDefStm (SubDef name _ _ _ _))
		= Just name
declaredType (ClassDefStm classDef)
		= Just $ name classDef
declaredType _	= Nothing




-----------
-- UTILS --
-----------

allTypes	:: (Name, [Type], [TypeRequirement]) -> [Type]
allTypes (_,tps,treqs)
		= tps ++ map snd treqs

-- graph operator
addAll		:: (Ord k, Ord v) => [(k,v)] -> Map k (Set v) -> Map k (Set v)
addAll pairs dict
		= Prelude.foldr addOne dict pairs
-- graph operator
addOne	:: (Ord k, Ord v) => (k,v) -> Map k (Set v) -> Map k (Set v)
addOne (k,v) dict
		= M.insert k (S.insert v $ findWithDefault S.empty k dict) dict

err str fqn 	= error $ "Building type lookup table: fqn not found: "++show fqn++" within "++str++" table"
fwd str fqn	= findWithDefault (err str fqn) fqn
