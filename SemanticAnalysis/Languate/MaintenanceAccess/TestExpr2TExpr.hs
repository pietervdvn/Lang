module Languate.MaintenanceAccess.TestExpr2TExpr where

{--
This module builds all the stuff!
--}

import qualified Bnf
import Bnf.ParseTree
import Exceptions


import Languate.File2Package
import Languate.FQN
import Languate.AST
import Languate.Package
import Languate.MarkUp

import System.IO.Unsafe
import System.Directory

import Languate.TableOverview
import Languate.ParserStub
import Languate.FunctionTable
import Languate.FunctionTable.TypeExpr
import Languate.FunctionTable.TypePattern
import Languate.Precedence.Expr2PrefExpr
import Languate.TAST
import Data.Map
import Data.Maybe
import StdDef
import StateT
import Languate.MaintenanceAccess.TestBind (pt, pr, to)
import Languate.MaintenanceAccess.TestBuild

import qualified Data.Map as M
import qualified Data.Set as S

defFrees	= [] -- "a","b","pubKey","privKey"]

tp	= tst "mapl (!) (Elem True Empty)" |> head & unsafePerformIO



tst str	= do	expr	<- parseExpr str |> expr2prefExpr (precedenceTable tablesOverv)
		result	<- runExceptionsIO' $ expr2texpr loadedPackage tablesOverv prelude defFrees M.empty expr
		putStrLn "-- original expression --"
		print expr
		putStrLn "-- which has the type --"
		return $ result |> typeOf

parseExpr s	= parse "expr" s 	|> pt2expr
parsePattern s	= parse "patternRoot" s |> pt2pattern



-- builds our parse tree
parse	:: String -> String -> IO ParseTree
parse rule str
		=  do	let mpt	= Bnf.parseFull bnfs (Bnf.toFQN ["Languate"]) rule str
			let pt' = fromMaybe (error "Incorrect parse, not even a single character could be parsed!") mpt
			let pt  = case pt' of
					Right pt	-> pt
					Left exception	-> error $ show exception
			return pt
