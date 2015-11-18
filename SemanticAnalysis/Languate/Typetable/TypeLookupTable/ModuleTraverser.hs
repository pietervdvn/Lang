module Languate.Typetable.TypeLookupTable.ModuleTraverser where


import StdDef
import Languate.AST

import Control.Applicative



-- function declarations in module, which are public/private
functions	:: Visible -> Module -> [(Name, [Type], [TypeRequirement])]
functions mode mod
		= let 	restrict	= exports mod
			stms	= statements mod in
		  _censor ((mode == Public) ==) restrict $ concatMap _unpackF stms

_censor		:: (Bool -> Bool) -> Restrict -> [(Name, [Type], [TypeRequirement])]
			-> [(Name, [Type], [TypeRequirement])]
_censor inv restrict
		= filter (\(nm,_,_) -> inv $ isAllowed restrict nm)


_unpackF	:: Statement -> [(Name,[Type], [TypeRequirement])]
_unpackF (FunctionStm f)
		= signs f |> (\(nm,ts,tr) -> (nm, [ts], tr))
_unpackF (ClassDefStm cd)
		= (\(nm,ts,tr) -> (nm,ts,tr)) <$> decls cd
_unpackF _	= []
