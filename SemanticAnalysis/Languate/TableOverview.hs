module Languate.TableOverview where

{--
This module builds all the tables you'll ever need!
--}

import StdDef
import Languate.FQN
import Languate.World
import Languate.TypeTable
import Languate.TypeTable.BuildTypeLookupTable
import Languate.TypeTable.BuildTypeTable
import Languate.TypeTable.TypeTable2MD

import Languate.Precedence.PrecedenceTable
import Languate.Precedence.PrecTable2MD

import Languate.CheckUtils


data TableOverview	= TableOverview { typeTable		:: TypeTable
					, precedenceTable	:: PrecedenceTable}

buildAllTables	:: World -> Exc TableOverview
buildAllTables w	= do	tt	<- buildTypeTable w
				precT	<- buildPrecTable' w
				return $ TableOverview tt precT
