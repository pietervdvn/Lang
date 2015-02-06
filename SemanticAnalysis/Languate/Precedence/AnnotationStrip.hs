module Languate.Precedence.AnnotationStrip where

{--
This module implements a help function, which gets the needed info to build the precedence table
--}
import StdDef
import Languate.AST
import Data.Maybe

import Control.Arrow

getPrecedenceInfo'	:: Statement -> Maybe ((Name, PrecModifier), [PrecRelation])
getPrecedenceInfo' (PrecedenceStm (PrecAnnot name modif rels))
			= Just ((name, modif), rels)
getPrecedenceInfo' _	= Nothing


getPrecedenceInfo	:: Module -> ([(Name, PrecModifier)], [PrecRelation])
getPrecedenceInfo	=  second concat . unzip . mapMaybe getPrecedenceInfo' . statements

ltRelations	:: [PrecRelation] -> [(Name, Name)]
ltRelations rls	=  filter (not . isEqRel) rls |> opsIn

eqRelations	:: [PrecRelation] -> [(Name,Name)]
eqRelations rls	=  filter isEqRel rls |> opsIn

opsIn		:: PrecRelation -> (Name, Name)
opsIn (PrecLT a b)	= (a,b)
opsIn (PrecEQ a b)	= (a,b)

opsIn'		:: PrecRelation -> [Name]
opsIn' precR	=  let (a,b)	= opsIn precR in
			[a,b]

isEqRel		:: PrecRelation -> Bool
isEqRel (PrecEQ _ _)
		= True
isEqRel	_	= False
