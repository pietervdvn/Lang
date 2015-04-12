module Languate.MaintenanceAccess.TestExpr2TExpr where

{--
This module builds all the stuff!
--}

import qualified Bnf
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
import Languate.FunctionTable.Expr2Texpr
import Languate.Precedence.Expr2PrefExpr
import Data.Map
import Data.Maybe
import StdDef

bnfs		= unsafePerformIO $ Bnf.load "../Parser/bnf/Languate"
path		= "../workspace/Data"
packageIO	= loadPackage' bnfs (toFQN' "pietervdvn:Data:Prelude")
loadedPackage	= unsafePerformIO $ packageIO path
tablesOverv	= unsafePerformIO $ runExceptionsIO' $ buildAllTables loadedPackage


t	= tst "True"

tst str	= do	expr	<- parseExpr str |> expr2prefExpr (precedenceTable tablesOverv)
		result	<- runExceptionsIO' $ expr2texpr loadedPackage tablesOverv
			(toFQN' "pietervdvn:Data:Prelude") expr
		putStrLn "-- original expression --"
		print expr
		putStrLn "-- which has the type --"
		print result

bDocs	= do	dir	<- getCurrentDirectory
		let cluster	= buildCluster []
		let cluster'	= add tablesOverv cluster
		renderClusterTo (addFooter html)
				 (dir ++"/" ++ path ++ "/.gen" ++ "/html2") cluster'
		renderClusterTo (addFooter md) (dir ++"/" ++ path ++ "/.gen" ++ "/md2") cluster'


addFooter	:: RenderSettings -> RenderSettings
addFooter rs	= let back = InLink (Base "Back to all pages") "All pages" in
			rs {preprocessor = preprocess
				(\mu -> parags [back,mu,back]) . preprocessor rs}

-- builds our parse tree
parseExpr	:: String -> IO Expression
parseExpr str
		=  do	let mpt	= Bnf.parseFull bnfs (Bnf.toFQN ["Languate"]) "expr" str
			let pt' = fromMaybe (error "Incorrect parse, not even a single character could be parsed!") mpt
			let pt  = case pt' of
					Right pt	-> pt
					Left exception	-> error $ show exception
			return $ pt2expr pt
