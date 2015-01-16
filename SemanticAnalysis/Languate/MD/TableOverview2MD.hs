module Languate.MD.TableOverview2MD where


import StdDef
import MarkDown
import Data.Map (Map, elems)

import Languate.FQN
import Languate.World
import Languate.TypeTable
import Languate.TableOverview
import Languate.TypeTable.TypeTable2MD
import Data.List (nub)

import Languate.MD.TodoMD

import Languate.MD.MDGen

import Languate.CheckUtils

-- Writes the overview tables to the relevant .md and .html
writeTables	:: World -> TableOverview -> FilePath -> IO ()
writeTables w to packagePath
		=  do	let path	= packagePath++"/.gen/"
			let save nm mdGen table	= saveTo path (nm++"Overview") $ generate (nm++" overview") $ mdGen $ table to
			save "Operator" show precedenceTable
			save "Type" typeTable2md typeTable
			let todoT	= buildTodoTable w
			let nrTodo	= length $ concat $ elems todoT
			saveTo path "Todo" $ generate' "Issues" (todoTable2md todoT)
			let index	= generate' "Index" $ indexMD nrTodo
			saveTo path "Index" index


indexMD	:: Int -> (String -> String -> String) -> MarkDown
indexMD todo link
	=  concatMap parag $
			[ link "Type Overview" "TypeOverview"
			, link "Operator Overview" "OperatorOverview"] ++
			if todo == 0 then []
				else [link (plural todo "todo") "Todo"]
