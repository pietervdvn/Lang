module Languate.MaintenanceAccess.TestSemantal where

import System.IO.Unsafe

import Prelude hiding (lookup, Left, Right)
import Data.Map hiding (map)
import Data.Maybe
import Control.Monad.Reader

import Languate.File2Package
import Languate.FQN
import Languate.SymbolTable
import Languate.AST
import Languate.BuiltIns
import Languate.TypeChecker
import Languate.Order


{--
Dev code for semantic analysis.
--}

package	= unsafePerformIO $ loadPackage' (toFQN' "pietervdvn:Data:Prelude") "../workspace/Data/src/"

bool	= toFQN' "pietervdvn:Data:Data.Bool"
bool'	= fromJust $ lookup bool package

testBuild	= buildWithImports package


-- the environment
tt	= TT Empt $ fromList $ [("f", [ [nat,nat] --> nat, [nat] --> nat , [nat,nat] --> (asMaybe nat)])
				, (".", [dotType])
				, ("c", [nat])
				, ("nat2int", [ [nat] --> int] )
				] ++ map (\op -> (op, [[nat,nat] --> nat])) ["+","-","*","/","%"]
dotType	= Curry [Free "a", Curry [Free "a", Free "b"], Free "b"]


tctx	= Context tt priorTable
-- type check test

tc	:: Expression -> TypedExpression
tc expr	= runReader (typeCheck expr) tctx


priorTable	:: PriorityTable
priorTable	= fromList [("+",(30, Left)),("-",(30, Left)),("*",(20, Left)),("/",(20, Left)),("%",(20, Left)), ("²", (15, Right)), (".",(10,Right)), (">>", (15, Right)), ("<<", (15, Left)) ]

testExprs	= map Seq [[Call "f", Nat 1, Nat 2]
			  ,[Call "c", Operator "."]
			  ,[Call "c", Operator ".", Call "nat2int"]
			  ,[Nat 1, Operator "+", Nat 2]
			  ,[Nat 1 , Operator "+", Nat 2, Operator "*", Nat 3],[Nat 1, Operator "²"],[Nat 2, Operator "/"]	,[Operator "/"],[Call "f", Call "x", Operator "+", Call "entity", Operator ".", Call "point", Operator ".", Call "x"],[Call "f", Operator ">>", Call "g", Operator ">>",Operator "h"],[Call "f", Operator "<<", Call "g", Operator "<<", Operator "h"]]

-- following expressions correctly generate an error
crashinExprs	= map Seq [[Operator "²", Nat 1], [Operator "/", Nat 2]]


order expr	=  runReader (asCall expr) priorTable

t 		= map tc testExprs


{--- 

Point x:Int y:nt
Entity pos:Point

p.x	: int
p.x.fac =  fac p.x

x	: Point -> Int
x	: (Int -> Int) -> Point -> Point
p.x.(+1)	= 
p.(x.(+1))


e.pos.x	: Entity -> Int
e.pos.x	: (Int -> Int) -> (Entity -> Entity)

e.pos.x + 4	= x (pos (e)) + 4	-- take value, add 4, done
e.pos.x.+ 4	=			-- create new entity which is 4 to the right




--}
