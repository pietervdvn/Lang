module Languate.MaintenanceAccess.TestLocalScope where

import StdDef

import Languate.MaintenanceAccess.TestBuild
import Languate.Package
import Languate.AST
import Languate.TableOverview
import Languate.FunctionTable
import Languate.TAST

import Data.Map as M

t	= loadedPackage & modules
		& M.findWithDefault (error "TLS: ???") bool
