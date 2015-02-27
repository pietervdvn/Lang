module Languate.MD.TodoMD where

{--
Creates a TO DO-list!
--}

import StdDef
import MarkDown
import Languate.FQN
import Languate.Package
import Languate.AST
import Data.Map hiding (mapMaybe, map, null)
import qualified Data.Map as M
import Languate.TableOverview

import Regex
import ConsumerL
import Data.Maybe
import Data.Char
import Data.List
import Data.Either

keywords	= ["todo","fixme","fix me","fix-me","wtf"]
-- Table per module, what type ("todo","fixme") and message. Todos consist of one line!
type TodoTable	= Map FQN [(Coor, Name, String)]

buildTodoTable	:: Package -> TodoTable
buildTodoTable w
		=  M.filter (not . null) $ mapWithKey todoFor $ modules w



todoFor		:: FQN -> Module -> [(Coor, Name, String)]
todoFor fqn m	=  let  importTodo	= imports m & lefts & Comments & flip todoIn (0,0)
			stmTodo		= statements' m >>= uncurry todoIn in
			importTodo ++ stmTodo

todoIn		:: Statement -> Coor -> [(Coor, Name, String)]
todoIn (DocStringStm docs) coor
		= todoIn (Comments $ map comment docs) coor
todoIn (Comments coms) coor
		= do	str	<- coms
			(todo, kw)	<- actualTodo str
			let todo'	= strip $ drop (length kw) todo
			return (coor, kw, todo')

todoIn _ _	= []


actualTodo	:: String -> [(String, String)]
actualTodo str	=  do	kw	<- keywords
			todo	<- actualTodo' kw str
			return (todo, kw)


actualTodo'	:: String -> String -> [String]
actualTodo' keyword comment
	=let	caseIns = concatMap (\c -> '[':toLower c:toUpper c : "]")
		rgx	= regex $ caseIns keyword ++"!\n*" in
		mapMaybe (longestMatch rgx) $ lines comment

todoTable2md	:: TodoTable -> (String -> String -> String) -> MarkDown
todoTable2md tt link
		= table ["Module", "Line", "Type","Message"] $
			 concatMap (\(fqn, ls) -> map (showLine fqn) ls) $ toList tt

showLine	:: FQN -> (Coor, Name, String) -> [MarkDown]
showLine fqn (coor, nm, str)
		= [show fqn, show $ fst coor, nm, str]
