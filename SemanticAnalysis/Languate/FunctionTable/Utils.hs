module Languate.FunctionTable.Utils where

import StdDef
import HumanUtils
import Languate.MarkUp as Mu

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
		docstrRow (sign, meta)
			= [code $ signName sign
				, code $ show $ signFQN sign
				, meta & metaDefined & fst & show & code
				, meta & metaComments |> fst |> code & parags
				, meta & metaDocs |> fst |> (\((name, f), comm) -> code name+++code f+++code comm) & Mu.Seq
				, meta & metaAnnots |> fst |> (\(Annotation nm str) -> code (nm++":") +++ Base str) & Mu.Seq
				]
		docstrT	= table ["Defined function","Defining module","Line","Comments","Docs","Annotation"] (ft & documentation & M.toList |> docstrRow)
		docstrs	= titling "Function information" docstrT	:: MarkUp
		docs	= [doc (path fqn ++"Function signatures of "++show fqn) "Signatures" defT,
				 doc (path fqn ++"Meta info about functions of "++show fqn) "Docstrings and such" docstrs]
		in
		(doc (path fqn ++"Functiontable of "++show fqn) ("Function overview for "++show fqn) (docs |> title |> Embed & Mu.Seq),
			docs)
