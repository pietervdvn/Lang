module Languate.MarkUp.Doc where

import StdDef
import Languate.MarkUp.MarkUp
import Data.Map

data Doc = Doc {title::String, description::String, meta::Map Name String, contents::MarkUp}


