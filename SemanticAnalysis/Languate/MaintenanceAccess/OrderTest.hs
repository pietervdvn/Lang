module Languate.MaintananceAccess.OrderTest where

-- default priority table to test stuff
import Languate.Order
import Data.Map
import Control.Monad.Reader
import Languate.AST
import Prelude hiding (Left, Right)

priorTable	:: PriorityTable
priorTable	= fromList [("+",(30, Left)),("-",(30, Left)),("*",(20, Left)),("/",(20, Left)),("%",(20, Left)), ("²", (15, Right)), (".",(10,Right)), (">>", (15, Right)), ("<<", (15, Left)) ]

t1		=  t [Call "f", Nat 1, Nat 2]
t2		=  t [Call "f", Operator "."]
t3		=  t [Nat 1, Operator "+", Nat 2]
t4		=  t [Nat 1 , Operator "+", Nat 2, Operator "*", Nat 3]
t5		=  t [Nat 1, Operator "²"]	-- postrfix operator test
t5'		=  t [Operator "²", Nat 1]	-- postfix as prefix, not allowed
t6		=  t [Operator "/", Nat 2]	-- not allowed
t7		=  t [Nat 2, Operator "/"]	-- 2 divided by something
t8		=  t [Operator "/"]		-- (/)
t9		=  t [Call "f", Call "x", Operator "+", Call "entity", Operator ".", Call "point", Operator ".", Call "x"]
t10		=  t [Call "f", Operator ">>", Call "g", Operator ">>", Operator "h"]
t11		=  t [Call "f", Operator "<<", Call "g", Operator "<<", Operator "h"]


t exprs		=  runReader (asCall $ Seq exprs) priorTable

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
