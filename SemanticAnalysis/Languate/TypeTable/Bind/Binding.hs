module Languate.TypeTable.Bind.Binding where
{--
This module implements needed datastructures + utils
--}

import StdDef

import Data.Map (Map)
import qualified Data.Map as M

import Languate.TAST
import Languate.TypeTable


data Binding	= Binding (Map Name RType)	-- data type, and not a type alias to allow a custom show function



-- UTILS

instance Show Binding where
	show	= sb

sb (Binding b)
	= sd b


sd	:: (Show k, Show v) => Map k v -> String
sd  d
	= "{"++unwords (map (\(k, v) -> show k ++" --> "++show v) $ M.toList d)++"}"

noBinding	= Binding M.empty
