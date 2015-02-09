module Languate.MaintenanceAccess.TestSuperType where

{--

This module implements tests for the super type relationship.

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
import Languate.World

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
import Data.Either

import qualified Bnf

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
bnd t0 t1	= test (pt t0) (pt t1) $ fromList $ merge (pr t0 ++ pr t1)
-- What are the supertypes of a type?




tpabb t0 t1	= let 	t0'	= pt t0
			t1'	= pt t1	in
			runstateT (bapp t0' t1') $ Context empty tt noBinding


test t0 t1 reqs = bind tt reqs t0 t1

t'	= [10..13] |> (\i -> (i,i)) ||>> _t |> (\(i,r) -> r >>= (\v -> return (i,v)))
--  show i ++" "++ show r ++ "\n") & unlines & putStrLn

t	= mapM_ putStrLn (t' & lefts |> (++)"\n\n")

-- basic test
_t 0	= bnd "Nat" "(a:Eq)"
-- fails
_t 1	= bnd "Any" "(a:Eq)"
-- no binding (but does not fail)
_t 2	= bnd "Nat" "Any"
-- fails
_t 3	= bnd "Any" "Nat"
-- binds {a --> natT, b --> intT} by recursive binding
_t 4	= bnd "Curry Nat Int" "a -> b"
-- binds {a --> List Nat, b --> Nat}. The "b" is bound via the type requirements
_t 5	= bnd "List Nat" "(a:List b)"

-- simple curry binding
_t 6	= bnd "Nat -> Int" "a -> b"
-- conflicting binding for a
_t 7	= bnd "Nat -> Int" "a -> a"
-- Binding in, a--> intT
_t 8	= bnd "Nat -> Int" "Int -> a"

-- Binding via application
_t 9	= bnd "List Nat" "List a"

-- Advanced binding
_t 10	= bnd "List (Nat, Bool)" "Dict a b"
-- with set, as the requirement Eq applies
_t 11	= bnd "{IntInf}" "a0*"
_t 12	= bnd "{{Eq}}" "(a*)*"
_t 13	= bnd "RSA" "PubPrivAlgo (a:PrivateKey) (b:PublicKey)"

--
