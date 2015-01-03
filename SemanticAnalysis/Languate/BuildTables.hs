module Languate.BuildTables where

{--
This module builds all the tables you'll ever need!
--}

import StdDef
import Data.Map (Map)
import Languate.FQN
import Languate.World
import Languate.TypeTable
import Languate.TypeTable.BuildTypeLookupTable
import Languate.TypeTable.BuildTypeTable
import Languate.TypeTable.TypeTable2MD

import Languate.Precedence.PrecedenceTable
import Languate.Precedence.PrecTable2MD


import Languate.MD.MDGen

import Languate.Checks.CheckUtils


data TableOverview	= TableOverview { tlts			:: Map FQN TypeLookupTable
					, typeTable		:: TypeTable
					, precedenceTable	:: PrecedenceTable}

buildAllTables	:: World -> Exc TableOverview
buildAllTables w	= do	let tlts	=  buildTLTs w
				tt	<- buildTypeTable w tlts
				precT	<- buildPrecTable' w
				return $ TableOverview tlts tt precT


-- Writes the overview tables to the relevant .md
writeTables	:: TableOverview -> FilePath -> IO ()
writeTables to packagePath
		=  do	let genPath str	= packagePath++"/.gen/"++str++"Overview.md"
			let save nm mdGen table	= saveTo (genPath nm) $ generate (nm++" overview") $ mdGen $ table to
			save "Operator" show precedenceTable
			save "Type" typeTable2md typeTable
