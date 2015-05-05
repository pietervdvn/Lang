module Languate.TypeTable.BuildDocstringTable where

{--
This module implements the function which gets the docstring for each type.
--}

import StdDef
import Exceptions
import Languate.CheckUtils


import Languate.AST hiding (docstringFor)
import Languate.FQN
import Languate.Package
import Languate.TypeTable

import Data.Map hiding (mapMaybe, map)
import Prelude hiding (lookup)
import Data.Set (Set)
import Data.Maybe
import Data.Tuple

import Debug.Trace

buildDocstringTable	:: Package -> [TypeID] -> Exc (Map TypeID String)
buildDocstringTable w typeIds
		= docstringsFor w typeIds |> fromList


docstringsFor	:: Package -> [TypeID] -> Exc [(TypeID, String)]
docstringsFor w typeIds
		= do	docs	<- mapM (try' Nothing . docstringFor w) typeIds
			zip docs typeIds & unpackMaybeTuples |> swap
				||>> strip ||>> stripnl & return


docstringFor	:: Package -> TypeID -> Exc (Maybe Comment)
docstringFor w (fqn, name)
	= do	modul		<- lookup fqn (modules w) ?
					("Bug: Module "++show fqn ++" not found.")
		let mDocstr	= searchCommentAbove modul (declaresType name)
		assert' (isJust mDocstr) $ "No docstring found for "++show fqn ++"." ++ show name
		return mDocstr
