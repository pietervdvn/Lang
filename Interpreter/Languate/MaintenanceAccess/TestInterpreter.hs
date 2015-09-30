module Languate.MaintenanceAccess.TestInterpreter where

import qualified Bnf
import Bnf.ParseTree
import StdDef
import Languate.CheckUtils
import Exceptions


import Languate.File2Package
import Languate.FQN
import Languate.AST
import Languate.Package
import Languate.ParserStub

import System.IO.Unsafe
import System.Directory
import Data.Maybe

import Languate.TableOverview
import Languate.BuildTableOverview
import Languate.Semantal
import Languate.TAST

import Languate.Interpreter
import Languate.Value
import HumanUtils

import Data.List
import Data.Map as M

-----------------------------------
-- i : Parse, type and Interpret --
-----------------------------------
i	:: String -> IO [String]
i str'	=  do	let str	= "stringify "++pars str'
		expr	<- parseExpr str
		print expr
		texprs	<- runExceptionsIO' $ inside "interactive:" $ inside ("While typing "++show expr) $
				 typeExpr loadedPackage tablesOverv prelude [] M.empty expr
		let context	= Ctx tablesOverv prelude M.empty []
		let val		= texprs |> evalExpr context
		return $ val |> showStringValue


t	:: String -> IO ()
t str	=  do	texprs <- typedExpr str
		texprs |> typeOf & nub |> show |> ("\t"++) & unlines & ("Possible types are:\n"++) & putStrLn


typedExpr	:: String -> IO [TypedExpression]
typedExpr str
	= do	expr	<- parseExpr str
		runExceptionsIO' $ inside "interactive:" $ inside ("While typing "++show expr) $
				 typeExpr loadedPackage tablesOverv prelude [] M.empty expr

info	= [("i 'expression'", "Interpret given expression"),("t 'expression'","Type given expression"),("parse 'bnf-rule' 'expression'", "Parse given expression to it's parse tree'"),("typedExpr 'expression'", "Parse and type expression, give back in a typed form"), ("parseExpr 'expression'","Parse expression"),("help","Give this help")]

help	= info |> (\(a,b) -> "  "++ a ++ "\t"++b) & unlines & ("\nInterpreter help:\n-----------------\n\n"++) & putStrLn

bnfs		= unsafePerformIO $ Bnf.load "../Parser/bnf/Languate"
path		= "../workspace/Data"
packageIO	= loadPackage' bnfs (toFQN' "pietervdvn:Data:Prelude")
prelude		= toFQN' "pietervdvn:Data:Prelude"

loadedPackage	= unsafePerformIO $ packageIO path
tablesOverv	= unsafePerformIO $ runExceptionsIO' $ buildAllTables loadedPackage

parseExpr s	= parse "expr" s 	|> pt2expr

-- builds our parse tree
parse	:: String -> String -> IO ParseTree
parse rule str
		=  do	let mpt	= Bnf.parseFull bnfs (Bnf.toFQN ["Languate"]) rule str
			let pt' = fromMaybe (error "Incorrect parse, not even a single character could be parsed!") mpt
			let pt  = case pt' of
					Right pt	-> pt
					Left exception	-> error $ show exception
			return pt
