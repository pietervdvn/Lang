module Languate.MarkUp.Doc where

import StdDef
import Languate.MarkUp.MarkUp
import Data.Map

data Doc = Doc	{title::String
		, description::String
		, meta::Map Name String
		, contents::MarkUp}


preprocess	:: (MarkUp -> MarkUp) -> Doc -> Doc
preprocess f dc	=  dc { contents = f $ contents dc}
