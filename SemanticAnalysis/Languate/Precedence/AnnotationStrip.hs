module Languate.Precedence.AnnotationStrip where

{--
This module implements a help function, which gets the needed info to build the precedence table
--}
import StdDef
import Languate.AST
import Data.Maybe

import Control.Arrow

getPrecedenceInfo'	:: Statement -> Maybe ((Name, PrecModifier), [PrecRelation])
getPrecedenceInfo' (AnnotationStm (PrecAnnot name modif rels))
			= Just ((name, modif), rels)
getPrecedenceInfo' _	= Nothing


getPrecedenceInfo	:: Module -> ([(Name, PrecModifier)], [PrecRelation])
getPrecedenceInfo mod	=  second concat $ unzip $ catMaybes $ map getPrecedenceInfo' $ statements mod

ltRelations	:: [PrecRelation] -> [(Name, Name)]
ltRelations 	=  map (\(PrecLT o1 o2) -> (o1, o2)) . filter (not . isEqRel)

eqRelations	:: [PrecRelation] -> [(Name,Name)]
eqRelations	=  map (\(PrecEQ o1 o2) -> (o1,o2)) . filter isEqRel

isEqRel		:: PrecRelation -> Bool
isEqRel (PrecEQ _ _)
		= True
isEqRel	_	= False
