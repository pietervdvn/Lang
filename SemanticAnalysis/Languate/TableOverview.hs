module Languate.TableOverview where

{--
This module builds all the tables you'll ever need!
--}

import StdDef
import Languate.FQN
import Languate.Package
import Languate.TypeTable
import Languate.TypeTable.BuildTypeLookupTable
import Languate.TypeTable.BuildTypeTable
import Languate.TypeTable.TypeTable2mu

import Languate.Precedence.PrecedenceTable
import Languate.Precedence.BuildPrecedenceTable

import Languate.FunctionTable
import Languate.FunctionTable.BuildFunctionTable

import Languate.CheckUtils

import Languate.MarkUp as Mu

import Control.Applicative


data TableOverview	= TableOverview { typeTable		:: TypeTable
					, functionTables	:: FunctionTables
					, precedenceTable	:: PrecedenceTable}


buildAllTables	:: Package -> Exc TableOverview
buildAllTables w	= do	tt	<- buildTypeTable w
				precT	<- buildPrecTable' w
				(fts, rawClauses)
					<- buildFunctionTables w tt
				return $ TableOverview tt fts precT


instance Documentable TableOverview where
	toDocument to	= (doc "Table overview" "All generated tables" $ Mu.Seq (["Type overview", "Precedence Overview"] |> Embed),
			addDocs' (precedenceTable to) $ addDocs' (functionTables to) $ addDocs' (typeTable to) [])
