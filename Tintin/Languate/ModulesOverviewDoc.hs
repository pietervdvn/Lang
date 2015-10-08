module Languate.ModulesOverviewDoc where

import StdDef
import Languate.MarkUp
import Languate.Package
import Languate.TableOverview
import Languate.AST

import Data.Map as M

modulesOverviewTitle	= "Modules"

modulesOverview	:: Package -> TableOverview -> Doc
modulesOverview p to
	= let mods	= p & modules & M.keys
				|> show |> (\n -> InLink (code n) ("Modules/"++n))
				|> (:[]) in
		doc modulesOverviewTitle "Overview of all modules" $ table ["Module"] mods
