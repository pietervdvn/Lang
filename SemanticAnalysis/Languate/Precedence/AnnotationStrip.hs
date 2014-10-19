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
