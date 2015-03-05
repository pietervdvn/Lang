module Languate.MaintenanceAccess.TestBind where

{--

This module implements tests for bind

USAGE:

bnd "Int" "a"	= bind "Int" in a
supers "Int"	= superTypes of "Int" -> Any, Eq, BIInt

Also see for a comprehensive type list:
workspace/Data/.gen/html/TypeOverview.html

--}


import StdDef
import qualified Bnf
import Exceptions


import Languate.File2Package
import Languate.FQN
import Languate.AST
import Languate.Package

import Languate.TableOverview
import Languate.MD.TableOverview2MD

import Languate.TypeTable.Bind

import Languate.TypeTable
import Languate.TAST

import System.IO.Unsafe
import System.Directory

import StateT
import Languate.ParserStub

import Data.Map (empty, fromList, findWithDefault)
import qualified Data.Set as S
import Data.List

-- So we can rebuild the docs
import Languate.MaintenanceAccess.TestBuild (t)

-- setup
bnfs		= unsafePerformIO $ Bnf.load "../Parser/bnf/Languate"
path		= "../workspace/Data"
packageIO	= loadPackage' bnfs (toFQN' "pietervdvn:Data:Prelude")
toIO	= do	world	<- packageIO $ path++"/src/"
		runExceptionsIO' $ buildAllTables world
to	= unsafePerformIO toIO
tt	= typeTable to
tlt	= findWithDefault (error "Prelude not found")
		(toFQN' "pietervdvn:Data:Prelude") $ typeLookups tt

-- parsing of types + ugly unpacking
str2type str	= let parsed	= Bnf.parseFull bnfs (Bnf.toFQN ["Types"]) "type" str in
			case parsed of
				Nothing	-> error $ "Could not parse "++str++" at all"
				Just (Left exc)	-> error $ show exc
				Just (Right pt)	-> pt2type pt

type2rtype t	= let	(_,_,mType)	= runExceptions $ resolveType tlt t in
			case mType of
				Right rt	-> rt
				Left msg	-> error msg

-- parse types
pt	= type2rtype . fst . str2type
-- parse reqs
pr	= (||>> type2rtype) . snd . str2type



---------------
-- TEST HERE --
---------------

-- Test binding of t0 in t1
bnd' t0 t1	= test (pt t0) (pt t1) $ fromList $ merge (pr t0 ++ pr t1)

bnd t0 t1	= case bnd' t0 t1 of
			Left msg	-> putStrLn msg
			Right bnd	-> print bnd

test t0 t1 reqs = bind tt reqs t0 t1

-- basic test
t0	= bnd "Nat" "(a:Eq)"
-- fails
t1	= bnd "Any" "(a:Eq)"
-- no binding (but does not fail)
t2	= bnd "Nat" "Any"
-- fails
t3	= bnd "Any" "Nat"
-- binds {a --> natT, b --> intT} by recursive binding
t4	= bnd "Curry Nat Int" "a -> b"
-- binds {a --> List Nat, b --> Nat}. The "b" is bound via the type requirements
t5	= bnd "List Nat" "(a:List b)"

-- simple curry binding
t6	= bnd "Nat -> Int" "a -> b"
-- conflicting binding for a
t7	= bnd "Nat -> Int" "a -> a"
-- Binding in, a--> intT
t8	= bnd "Nat -> Int" "Int -> a"

-- Binding via application
t9	= bnd "List Nat" "List a"

t10	= bnd "Int -> Bool" "a -> a"

-- binding via the super type tables
t11	= bnd "[Nat]" "{a}"
