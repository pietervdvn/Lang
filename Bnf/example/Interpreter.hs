module Interpreter where

{--
This module interprets a simple program in 'straightLine' 
--}

import State

import StdDef
import Data.Map
import Def

import Control.Arrow (second)

-- executes the program, gives the output
output		:: [Statement] -> [Int]
output stms	= snd $ snd $ runstate (interprets stms) (empty, [])

interprets	:: [Statement] -> State (Map Name Int, [Int]) ()
interprets	= mapM_ interpret

interpret	:: Statement -> State (Map Name Int, [Int]) ()
interpret (PrintStmt e)
		= do	(vars, output)	<- get
			let i = eval e vars
			put (vars, output++[i])
interpret (AssignStmt name e)
		= do	(vars, output)	<- get
			let i	= eval e vars
			put (insert name i vars, output)
			

eval			:: Expr	-> Map Name Int -> Int
eval (Integer i) _	= i
eval (Call name) mp	= findWithDefault (error $ "Name not defined: "++show name) name mp
eval (Add e1 e2) mp	= 	let i1	= eval e1 mp in
				let i2	= eval e2 mp in
				i1 + i2
