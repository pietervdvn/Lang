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
import Data.List (intercalate)


{--
Dev code for semantic analysis.

Contains some example expressions 
--}

package	= unsafePerformIO $ loadPackage' (toFQN' "pietervdvn:Data:Prelude") "../workspace/Data/src/"

bool	= toFQN' "pietervdvn:Data:Data.Bool"
bool'	= fromJust $ lookup bool package

-- the workspace functions, with all imports	
testBuild	= buildWithImports package


-- the environment (typetable)
tt	= TT Empt $ fromList $ [("f", [ [nat,nat] --> nat, [nat] --> nat , [nat,nat] --> asMaybe nat])
				, ("f1", [ [tEntity] --> nat ])
				, (".", [pipeType, dotType, dotType', dotTypeState])
				, ("c", [nat])
				, ("g", [nat])
				, ("nat2int", [ [nat] --> int] )
				, ("²", [ [nat] --> nat] )
				, ("x" , [ nat, int])
				, ("entity", [tEntity])
				, ("point", [ [tEntity] --> tPoint, [ [tPoint] --> tPoint, tEntity] --> tEntity ])
				, ("x", [ [tPoint] --> int, [[int] --> int, tPoint] --> tPoint])
				, ("p", [tPoint, [tPoint] --> tPoint])
				, ("|", [pipeType])	-- same as (.), but binds left
				, ("$", [revPipeType])
				] ++ map (\op -> (op, [[nat,nat] --> nat, [int,nat] --> int, [nat,int] --> int, [int,int] --> int])) ["+","-","*","/","%"]

-- a -> (a -> b) -> b
pipeType	= Curry [Free "a", Curry [Free "a", Free "b"], Free "b"]

-- (a -> b) -> a -> b
revPipeType	= [ [Free "a"] --> Free "b", Free "a"] --> Free "b"

-- a -> (b -> a -> a) -> b -> a
dotType	= [ Free "a", [Free "b", Free "a"] --> Free "a", Free "b"] --> Free "a"

-- (a -> b) -> (b -> c) -> a -> c
dotType'
	=[ [Free "a"] --> Free "b", [Free "b"] --> Free "c", Free "a"] --> Free "c"

-- ((b -> b) -> a -> a) -> ((c -> c) -> b -> b) -> a -> a
dotTypeState= [ [[Free "b"] --> Free "b", Free "a"] --> Free "a"
	  , [[Free "c"] --> Free "c", Free "b"] --> Free "b"
	  , [ Free "c"] --> Free "c"
	  ,   Free "a"] --> Free "a"


tEntity	= Normal "Entity"
tPoint	= Normal "Point"


tctx	= Context tt priorTable
-- type check test

tc	:: Expression -> TypedExpression
tc expr	= runReader (typeCheck expr) tctx


priorTable	:: PriorityTable
priorTable	= fromList [("+",(30, Left)),("-",(30, Left)),("*",(20, Left)),("/",(20, Left)),("%",(20, Left)), ("²", (15, Right)), (".",(10,Right)), ("|", (17, Left)), ("$", (100, Left)) ]

testExprs	= map Seq [[Call "f", Nat 1, Nat 2]
			  ,[Call "c", Operator "."]
			  ,[Call "nat2int", Call "c"]
			  ,[Call "c", Operator ".", Call "nat2int"]
			  ,[Nat 1, Operator "+", Nat 2]
			  ,[Nat 2, Operator "*", Nat 3]
			  ,[Nat 1 , Operator "+", Nat 2, Operator "*", Nat 3]
			  ,[Nat 1, Operator "²"]
			  ,[Nat 2, Operator "/"]
			  ,[Operator "/"]
			  ,[Call "entity", Operator ".", Call "point"]
			  ,[Call "point", Operator ".", Call "x"]
			  ,[Call "entity", Operator ".", Call "point", Operator ".", Call "x"]
			  ,[Call "f", Call "g", Operator "+", Call "entity", Operator ".", Call "point", Operator ".", Call "x"]
			  ,[Nat 1, Operator "|"]
			  ,[Nat 1, Operator "|", Call "²"]
			  ,[Nat 1, Operator "|", Call "²", Operator "|",Operator "²"]
			  ,[Call "²", Operator "$", Call "f1", Operator "entity"]]

-- following expressions correctly generate an error
crashinExprs	= map Seq [[Operator "²", Nat 1], [Operator "/", Nat 2]]


order expr	=  runReader (asCall expr) priorTable

t'		= map tc testExprs

t		= putStrLn $ unlines $ map (\(e, t) -> show e ++" : "++ show (typeOf t)) $ zip testExprs t'


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

{-
TApplication [(Entity -> Int),(Entity -> ((Int -> Int) -> Point))] 
(TCall [(a -> ((a -> b) -> b)),((a -> b) -> ((b -> c) -> (a -> c)))] ".") 
[TCall [(Entity -> Point),(Entity -> ((Point -> Point) -> Entity))] "point",
TCall [(Point -> Int),(Point -> ((Int -> Int) -> Point))] "x"]
-}

