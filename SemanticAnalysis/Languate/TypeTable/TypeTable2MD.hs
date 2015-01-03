module Languate.TypeTable.TypeTable2MD where

import StdDef
import MarkDown

import Data.Map (keys, Map, lookup)
import Prelude hiding (lookup)
import Languate.TypeTable


-- Build a simple markdown overview of the type table
typeTable2md	:: TypeTable -> MarkDown
typeTable2md tt	= (table ["Type","Declared in","Kind","Requirements","Docstring"] $ map (typeRow tt) $ keys $ kinds tt)

typeRow		:: TypeTable -> TypeID -> [MarkDown]
typeRow	tt (fqn, name)
		=  let getMaybe table	= lookup (fqn, name) $ table tt in
			[ name
			, show fqn
			, maybe (bold "ERROR: no kind found") show $ getMaybe kinds
			, bold "TODO"
			, maybe "" firstLine $ getMaybe docstrings
			]
