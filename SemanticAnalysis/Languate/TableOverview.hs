module Languate.TableOverview where

{--
This module builds all the tables you'll ever need!
--}

import StdDef
import Languate.FQN
import Languate.Package
import Languate.TypeTable
import Languate.TypeTable.TypeTable2mu
import Languate.Precedence.PrecedenceTable
import Languate.FunctionTable
import Languate.CheckUtils

import Languate.MarkUp as Mu

import Control.Applicative


data TableOverview	= TableOverview { typeTable		:: TypeTable
					, functionTables	:: FunctionTables
					, precedenceTable	:: PrecedenceTable
					, implementations	:: ImplementationTables}


instance Documentable TableOverview where
	toDocument to	= (doc "Table overview" "All generated tables" $ Mu.Seq (["Type overview", "Precedence Overview"] |> Embed),
			addDocs' (precedenceTable to)	$
			addDocs' (functionTables to) 	$
			addDocs' (typeTable to)		$
			addDocs' (implementations to)	$
			[])
