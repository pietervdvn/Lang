module Languate.Typetable.TypeLookupTable.TypeLookupTableDef where

import StdDef
import HumanUtils
import Languate.MarkUp as Mu

import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Tuple
import Data.List (sort)


import Languate.FQN

-- basically the same as the aliastable, but with types.
type TypeLookupTable	= Map ([Name], Name) (Set FQN)	-- mutliple values, multiple possiblities in some cases!



tlt2doc		:: String -> FQN -> TypeLookupTable -> Doc
tlt2doc title fqn tlt
		= let	mappings	= tlt |> S.toList & M.toList & unmerge |> swap & merge	:: [(FQN, [([Name],Name)])]
			mappings'	= mappings |||>>> (\(nms, nm) -> nms ++ [nm]) |||>>> intercal "." |> swap	:: [([String], FQN)]
			row (strings, fqn)	= [strings & sort |> code |> Parag & Mu.Seq, code $ show fqn] in
			doc (title ++ show fqn) ("What identifier does map on what type?") $
			table ["Allowed names", "Meaning"] (mappings' |> row)
