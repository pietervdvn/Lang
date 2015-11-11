module Languate.FunctionTable.Utils where

import StdDef
import HumanUtils
import Languate.MarkUp

import Languate.FQN
import Languate.FunctionTable.Def
import Languate.TAST

import Data.Map as M
import Data.Set as S


functiontable2doc	:: (FQN -> String) -> FQN -> FunctionTable -> (Doc, [Doc])
functiontable2doc path fqn ft
	= let	defRow (sign, visible)
			= [code $ signName sign, signTypes sign & show & code, Base $ show visible]
		defT	= table ["Defined functions","Types"] (defined ft & M.toList |> defRow)
		conts	= titling "Defined functions" defT
		in
		(doc (path fqn++"Functiontable of "++show fqn) ("Function overview for "++show fqn) conts,[])
