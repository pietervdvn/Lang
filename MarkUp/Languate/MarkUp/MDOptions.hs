module Languate.MarkUp.MDOptions where

{--
This module implements the md render options
--}

import Languate.MarkUp.Options
import Languate.MarkUp.MD

md	:: FilePath -> RenderSettings
md fp	= RenderSettings
		(localNamer fp ".md")
		fancyEmbedder
		renderDoc2MD
		(flip const)
		(Just defaultOverviewPage)
		[]
		(localNamer' fp)
