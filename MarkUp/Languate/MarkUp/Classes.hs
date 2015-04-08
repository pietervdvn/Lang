module Languate.MarkUp.Classes where

import Languate.MarkUp.MarkUp
import Languate.MarkUp.Doc

-- represents data that can be written as markup
class MarkUpable m where
	toMarkUp	:: m -> MarkUp

-- Something that can be made a document of
class Documentable m where
	toDocument	:: m -> (Doc, [Doc])

addDocs	:: (Documentable docable) => docable -> [Doc] -> [Doc]
addDocs docable docs
	= let (d, dcs)	= toDocument docable in
		d:dcs ++ docs
