module Languate.AliasTable (AliasTable, buildAliasTable, buildAliasTables, asNonDeterministicAT) where

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

{- The aliastable keeps track of name aliasses for modules.

E.g. "import Data.Map.Hashmap as M" will be saved as {["M"] --> "Data.Map.Hashmap"} into the aliastable.
Normal imports are saved too, but the entire path is saved:
	"import Data.Map.Hashmap" will be saved as {"Hashmap" --> <fqn>, "Map.Hashmap" --> <fqn>, "Data.Map.Hashmap" --> <fqn> }

When imports conflict, e.g. "import Package1.Data.Map" and "import Package2.Data.Map", the aliasmap will contain
	{"Map" -> Left [<fqn1>,<fqn2>],"Data.Map" -> Left [<fqn1>,<fqn2>],"Package1.Data.Map" -> Right <fqn1>,"Package2.Data.Map" -> Right <fqn2>}
In other words, a left when the name resolution is ambigous, a right when just a single FQN matches the name
-}
type AliasTable	= Map [Name] (Either [FQN] FQN)

asNonDeterministicAT	:: AliasTable -> Map [Name] [FQN]
asNonDeterministicAT	=  Data.Map.map unpack
				where 	unpack (Right fqn)	= [fqn]
					unpack (Left fqns)	= fqns

-- The names a certain FQN is known as
reversedAT		:: AliasTable -> Map FQN [Name]
reversedAT		=  foldr (\(path, fqns) ->  ) empty . toList

buildAliasTables	:: Map FQN (Set (FQN, Import)) -> Map FQN AliasTable
buildAliasTables	=  Data.Map.map buildAliasTable

-- builds the alias table for a single module
buildAliasTable	:: Set (FQN, Import) -> AliasTable
buildAliasTable imps
		= S.foldr (uncurry addImport) empty imps


addImport	:: FQN -> Import -> AliasTable -> AliasTable
addImport fqn (Import _ _ _ (Just alias) _) table
		= insertTable [alias] fqn table
addImport fqn (Import _ nms nm Nothing _) table
		= foldr (flip insertTable fqn) table $ init $ tails (nms ++ [nm])


insertTable	:: [Name] -> FQN -> AliasTable -> AliasTable
insertTable key val table
		= (\toInsert -> insert key toInsert table)
		 	(case lookup key table of
				Nothing		-> Right val
				Just found	-> _increase val found)


_increase	:: FQN -> Either [FQN] FQN -> Either [FQN] FQN
_increase fqn (Right fqn')
		= if fqn == fqn' then Right fqn' else Left [fqn, fqn']
_increase fqn (Left fqns)
		= Left $ nub $ fqn:fqns
