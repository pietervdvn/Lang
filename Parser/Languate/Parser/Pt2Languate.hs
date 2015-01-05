module Languate.Parser.Pt2Languate (pt2mod) where

import StdDef
import Bnf.ParseTree
import Bnf
import Bnf.Converter (advancedConvert, noAnnotations)
import Languate.Parser.Utils
import Languate.Parser.Pt2Import
import Languate.Parser.Pt2Comment
import Languate.Parser.Pt2Statement
import Languate.Parser.Pt2Law
import qualified Languate.AST as AST
import Languate.AST hiding (Line,Column)

import Control.Monad.Writer

{--

Where it all comes together.


This module converts the ParseTree into a full module.

--}

modName	= "Pt2Lang"

pt2mod	:: ParseTree -> Module
pt2mod	=  convert . fst . runWriter . advancedConvert (_modify h) t s

convert		:: CPT -> Module
convert cpt
		= let (Root annotCpts)	= cpt in
			applyAll annotCpts (Module "" (BlackList []) [] [])

applyAll	:: [(CPT, (Line,Column))] -> Module -> Module
applyAll stms	=  flip (foldr toMod) stms


toMod		:: (CPT,(Line,Column)) -> Module -> Module
toMod (Ident name, coor)
		= setname name
toMod (Exports exps, coor)
		= setExports exps
toMod (Nl,_)	= id
toMod (Comms c, coor)
		= addStm coor $ Comments c
toMod (Imps comms imp coor, _)
		= modImports $ (++) $ map Left comms ++ [Right (imp, coor)]
toMod (Stm stms, coor)
		=  addStms $ zip stms $ repeat coor
toMod (Root cpts, _)
		=  applyAll cpts


data CPT	= Ident Name
		| Exports [Name]
		| Comms [Comment]
		| Imp Import
		| Imps [Comment] Import (Line,Column)
		| Stm [Statement]
		| Root [(CPT, (Line, Column))]
		| Nl
	deriving (Show)


h		:: [(Name, ParseTree -> CPT)]
h		=  [ ("idSet", Exports . pt2idset)
		   , ("nlcomment", Comms . (:[]) . pt2comment)
		   , ("import", Imp . pt2imp)
		   , ("nls", Comms . pt2nls)
		   , ("statement", Stm . pt2stm)]

t		:: Name -> String -> CPT
t "globalIdent" id
		= Ident id
t _ "\n"	=  Nl
t nm cont	=  tokenErr modName nm cont


s		:: Name -> [([Name], CPT, (Line,Column))] -> CPT
s _ [(_,Comms coms,_),(_,Imp imp,coor)]
		= Imps coms imp coor
s _ [(_,Imp imp, coor)]
		= Imps [] imp coor
s _ [cpt]	= noAnnotations cpt
s _ cpts	= Root $ map (\(_,cpt, coor) -> (cpt, coor)) cpts
