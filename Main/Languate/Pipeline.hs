module Languate.Pipeline where

{--
This module implements the actual calls to the interpreter.
--}

import StdDef
import HumanUtils
import Languate.MarkUp as Mu
import State
import Exceptions
import Languate.CheckUtils

import Data.Maybe

import qualified Bnf
import Bnf.ParseTree

import Languate.File2Package
import Languate.FQN
import Languate.AST
import Languate.Package
import qualified Languate.Precedence.Expr2PrefExpr as Prefixer
import qualified Languate.ParserStub as ParserInternals

import Languate.TableOverview
import Languate.FunctionTable
import Languate.TAST
import Languate.BuildTableOverview
import Languate.Semantal

import Languate.TAST
import Languate.Tintin as Tintin

import qualified Data.Map as M

import System.IO.Unsafe
import Data.Time.Clock
import System.Directory

-- for now, the location of the files is hardcoded
bnfs		= unsafePerformIO $ Bnf.load "../Parser/bnf/Languate"	-- location of the bnf, needed for the parser
path		= "../workspace/Data"					-- location of the prelude/files project we work on
prelude		= toFQN' "pietervdvn:Data:Prelude"			-- location of the prelude, as FQN
packageIO	= loadPackage' bnfs (toFQN' "pietervdvn:Data:Prelude")



{-
State needed to do all the stuff
-}
data Context	= Context Package TableOverview


-- Loads (and forces!) all the files
loadContext	:: IO Context
loadContext	= do	package <- loadPackage' bnfs prelude path
			tablesOverv	<- runExceptionsIO' $ buildAllTables package
			return $ Context package tablesOverv


{-

The pipeline of an expression is:

   String
-> ParseTree	-- parse tokens, BNF-lib
-> AST		-- Abstract syntax tree, parser-lib
-> AST, prefix	-- Semantal
-> Typed Expression (TAST)	--  Semantal
-> Value	-- interpreter

All these steps are available from within the interpreter, and do need some form of state
-}


parseTree	:: String -> Either String ParseTree
parseTree str
	= do	let mpt	= Bnf.parseFull bnfs (Bnf.toFQN ["Languate"]) "expr" $ strip str
		case mpt of
			Nothing		-> Left $ "The expression "++show str++"' could not be parsed at all"
			Just (Left exc)	-> Left $ "The expression "++show str++"' could not be parsed:\n"++show exc
			Just (Right pt)	-> return pt


parseExpr	:: String -> Either String Expression
parseExpr str
		=  parseTree str |> ParserInternals.pt2expr

parsePrefExpr	:: Context -> String -> Either String Expression
parsePrefExpr (Context _ tablesOverv) str
		= parseTree str |> ParserInternals.pt2expr |> Prefixer.expr2prefExpr (precedenceTable tablesOverv)


parseTExpr	:: Context -> String -> IO [TypedExpression]
parseTExpr (Context package tablesOverv) str
		= do	expr	<- either error return $ parseExpr str
			-- crash if this fails, let's not worry about that here. Main should catch the error if needed
			runExceptionsIO' $ inside "interactive:" $ inside ("While typing "++show expr) $
				 typeExpr package tablesOverv prelude [] M.empty expr


info'	:: Context -> String -> IO String
info' ctx str
	= info ctx str |> renderString

info	:: Context -> String -> IO MarkUp
info ctx name
	= do	texprs	<- parseTExpr ctx name
		texprs |> infoAbout ctx & Mu.Seq & return

infoAbout	:: Context -> TypedExpression -> MarkUp
infoAbout (Context _ to) texpr@(TCall (typ, constr) sign)
	= do	let typeInf	= show typ ++ if null constr then "" else " where "++ show constr
		let docstring	= to & docstringTable & M.lookup sign |> Base & fromMaybe (Emph (Base "No docstring found"))
		let definedIn	= signFQN sign & show & ("Defined in "++) & Base
		Titling (Base $ "Info about " ++ show texpr ++ " : "++ typeInf) $ Mu.Seq [Parag definedIn, Parag docstring]


bDocs	:: FilePath -> Context -> IO ()
bDocs path (Context package tablesOverv)
	= do	dir	<- getCurrentDirectory
		time	<- getCurrentTime |> utctDayTime |> realToFrac |> round
		let (rendering, cluster)	= Tintin.generateDocs (show time) package tablesOverv
		let path'	= dir ++"/" ++ path ++ "/.gen" ++ "/html"
		let hour = 2 + time `div` (60*60)
		let css	= if hour `elem` ([0..8] ++ [21..24]) then blackCSS else defaultCSS
		removeDirectoryRecursive path'
		renderClusterTo (fix $ extend (setFilePath path' . rendering) $ html $ defaultHeader css) cluster
