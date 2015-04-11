module Languate.MaintenanceAccess.TestBuild where

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

import Data.Map
import Data.Maybe
import StdDef

bnfs		= unsafePerformIO $ Bnf.load "../Parser/bnf/Languate"
path		= "../workspace/Data"
packageIO	= loadPackage' bnfs (toFQN' "pietervdvn:Data:Prelude")
loadedPackage	= unsafePerformIO $ packageIO path
tablesOverv	= unsafePerformIO $ runExceptionsIO' $ buildAllTables loadedPackage

tst str	= do	expr	<- parseExpr str
		print $ expr2texpr loadedPackage tablesOverv 
			(toFQN' "pietervdvn:Data:Prelude") expr

bDocs	= do	dir	<- getCurrentDirectory
		let cluster	= buildCluster []
		let cluster'	= add tablesOverv cluster
		renderClusterTo html (dir ++"/" ++ path ++ "/.gen" ++ "/html2") cluster'
		renderClusterTo md (dir ++"/" ++ path ++ "/.gen" ++ "/md2") cluster'


-- builds our parse tree
parseExpr	:: String -> IO Expression
parseExpr str
		=  do	let mpt	= Bnf.parseFull bnfs (Bnf.toFQN ["Languate"]) "expr" str
			let pt' = fromMaybe (error "Incorrect parse, not even a single character could be parsed!") mpt
			let pt  = case pt' of
					Right pt	-> pt
					Left exception	-> error $ show exception
			return $ pt2expr pt

