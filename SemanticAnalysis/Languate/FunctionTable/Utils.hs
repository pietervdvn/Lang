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
	= let
	  -- about the declared functions
		defRow (sign, (visible, gen, abstract))
			= [code $ signName sign
				, signTypes sign & show & code
				, Base $ if isPublic visible then "" else "Private"
				, Base $ if gen then "Generated" else ""
				, abstract |> show & fromMaybe "" & Base ]
		defT	= table ["Defined functions","Types","Visible", "Generated","Defining type or category"] (definedFuncs ft & M.toList |> defRow)
		declaredDoc	= doc (path fqn ++"Function signatures of "++show fqn) "What functions are declared in this module?" defT


	  -- about the imported functions
		impRow	(name, typ, definedIn, importedBy)
			= [code name, code $ show typ, code $ show definedIn,
				importedBy |> show |> code & parags]
		importT	= table ["Function name","Function type", "Defined in", "Imported from (in this module)"]
				(ft & visibleFuncs |> S.toList & M.toList
					& unmerge |> (\(nm, (sign, imports)) -> (nm, signTypes sign, signFQN sign, imports))
					|> impRow)
		impDoc	= doc (path fqn ++ "Visible function signatures of "++show fqn) "What functions are visible in this module?"
				importT

	  -- about the implementations of locally declared functions
	  	implRow (sign, imp)
	  		= [code $ signName sign
	  		  , signTypes sign & fst |> show |> code & parags
	  		  , signTypes sign & snd & show & code
	  		  , imp |> show |> code & parags
	  		  ]
	  	implT	= table ["Function name","Types","Type constraints","Implementations"] (ft & implementations & M.toList |> implRow)
	  	implDoc	= doc (path fqn ++ "Implementions of functions in "++show fqn) "Internal table showing the actual implementations" implT

	  -- about the docstrings
		docstrRow (sign, meta)
			= [code $ signName sign
				, code $ show $ signFQN sign
				, meta & metaDefined & fst & show & code
				, meta & metaComments |> fst |> code & parags
				, meta & metaDocs |> fst |> (\((name, f), comm) -> code name+++code f+++code comm) & Mu.Seq
				, meta & metaAnnots |> fst |> (\(Annotation nm str) -> code (nm++":") +++ Base str) & Mu.Seq
				]
		docstrT	= table ["Defined function","Defining module","Line","Comments","Docs","Annotation"]
				(ft & documentation & M.toList |> docstrRow)
		docstrs	= titling "Function information" docstrT	:: MarkUp
		docDoc	= doc (path fqn ++"Meta info about functions of "++show fqn) "Docstrings and such" docstrs

	  -- Putting it all together
		docs	= [declaredDoc, impDoc, implDoc, docDoc]
		in
		(doc (path fqn ++"Functiontable of "++show fqn) ("Function overview for "++show fqn) (docs |> title |> Embed & Mu.Seq),
			docs)
