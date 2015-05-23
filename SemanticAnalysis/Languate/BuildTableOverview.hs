module Languate.BuildTableOverview where

import StdDef
import Languate.FQN
import Languate.Package
import Languate.TypeTable
import Languate.TypeTable.BuildTypeLookupTable
import Languate.TypeTable.BuildTypeTable
import Languate.TypeTable.TypeTable2mu
import Languate.TableOverview

import Languate.Precedence.PrecedenceTable
import Languate.Precedence.BuildPrecedenceTable

import Languate.FunctionTable
import Languate.FunctionTable.BuildFunctionTable
import Languate.FunctionTable.BuildImplementationTable

import Languate.CheckUtils

import Languate.MarkUp as Mu

import Control.Applicative
buildAllTables	:: Package -> Exc TableOverview
buildAllTables w
	= do	tt		<- buildTypeTable w
		precT		<- buildPrecTable' w
		(fts, rawClaus)	<- buildFunctionTables w tt
		-- implTable	<- buildImplementationTables w (TableOverview tt fts precT (error "You're building the IMP table dummy, don't try to fetch it!")) rawClaus
		return $ TableOverview tt fts precT todo -- implTable
