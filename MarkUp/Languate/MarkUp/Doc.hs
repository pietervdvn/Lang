module Languate.MarkUp.Doc where

import StdDef
import Languate.MarkUp.MarkUp
import Data.Map

data Doc = Doc	{title::String
		, description::String
		, meta::Map Name String
		, contents::MarkUp}

doc titl descr
	= Doc titl descr empty


preprocess	:: (MarkUp -> MarkUp) -> Doc -> Doc
preprocess f dc	=  let mu	= f $ contents dc in
			dc { contents = mu}
