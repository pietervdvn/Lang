module Languate.ModulesOverviewDoc where

import StdDef
import Languate.MarkUp
import Languate.Package
import Languate.TableOverview

modulesOverviewTitle	= "Exposed modules"

modulesOverview	:: Package -> TableOverview -> Doc
modulesOverview p to
	= doc modulesOverviewTitle "Overview of exposed modules" $ Base "TODO"
