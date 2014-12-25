module Languate.AliasTable (AliasTable, buildAliasTable, buildAliasTables) where

{--
Functions for calculating the alias table, given the import set
--}
import StdDef
import Languate.FQN
import Languate.AST
import Data.Set (Set)
import qualified Data.Set as S
import Data.List (tails, nub)
import Data.Map hiding (foldr)
import Prelude hiding (lookup)

{- The AliasTable keeps track of name aliasses for modules.

It saves what FQN is known under what path.

E.g. "import Data.Map.Hashmap as M" will be saved as {Data.Map.Hashmap --> ["M"]} into the AliasTable.
E.g. "import Data.Bool" will be saved as {Data.Bool -> ["Data","Bool"]}
-}
type AliasTable	= Map FQN [Name]

buildAliasTables	:: Map FQN (Set (FQN, Import)) -> Map FQN AliasTable
buildAliasTables	=  Data.Map.map buildAliasTable

-- builds the alias table for a single module
buildAliasTable	:: Set (FQN, Import) -> AliasTable
buildAliasTable imps
		= S.foldr (uncurry addImport) empty imps


addImport	:: FQN -> Import -> AliasTable -> AliasTable
addImport fqn (Import _ _ _ (Just alias) _) table
		= insert fqn [alias] table
addImport fqn (Import _ nms nm Nothing _) table
		= insert fqn (nms ++ [nm]) table
