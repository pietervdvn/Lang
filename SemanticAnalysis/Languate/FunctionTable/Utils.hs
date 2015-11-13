module Languate.FunctionTable.Utils where

import StdDef
import HumanUtils
import Languate.MarkUp

import Languate.FQN
import Languate.FunctionTable.Def
import Languate.TAST
import Languate.AST

import Data.Map as M
import Data.Set as S
import Data.Maybe

functiontable2doc	:: (FQN -> String) -> FQN -> FunctionTable -> (Doc, [Doc])
functiontable2doc path fqn ft
	= let	defRow (sign, (visible, gen, abstract))
			= [code $ signName sign
				, signTypes sign & show & code
				, Base $ if isPublic visible then "" else "Private"
				, Base $ if gen then "Generated" else ""
				, abstract |> show & fromMaybe "" & Base ]
		defT	= table ["Defined functions","Types","Visible", "Generated","Defining category"] (defined ft & M.toList |> defRow)
		conts	= titling "Defined functions" defT
		in
		(doc (path fqn++"Functiontable of "++show fqn) ("Function overview for "++show fqn) conts,[])
