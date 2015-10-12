module Languate.Typetable.BuildTypetable where


import StdDef

import Exceptions
import Languate.CheckUtils

import Languate.AST hiding (frees)
import Languate.TAST
import Languate.FQN

import Languate.Typetable.TypetableDef
import Languate.Typetable.TypeLookupTable
import Languate.Typetable.ModuleTraverser

import Languate.Checks.CheckType

import Data.Map as M
import Data.List as L

import Control.Arrow hiding ((+++))


{- builds the 'defined' type table from the code.
	-> Direct constraints on the free types are loaded
-}
buildTypetable	:: Module -> TypeLookupTable -> FQN -> Exc Typetable
buildTypetable mod tlt fqn
		= do	let locDecl	= locallyDeclared mod 	:: [(Name, [Name], [TypeRequirement])]
			locDecl |> buildTypeInfo tlt fqn & sequence |> M.fromList |> Typetable



buildTypeInfo	:: TypeLookupTable -> FQN -> (Name, [Name], [(Name, Type)]) -> Exc (TypeID, TypeInfo)
buildTypeInfo tlt fqn (n, frees, reqs)
		= inside ("While building type info about "++show fqn++"."++n) $
		  do	let indices	= zip frees [0..]	:: [(Name, Int)]
			reqs'		<- reqs |> (\(nm, t) -> do rt<-resolveType tlt t;return (nm, rt) ) & sequence	:: Exc [(Name, RType)]
			reqs' |> snd |> validateFrees frees & sequence
			let errMsg free	= "The free type variable "++free++" was not declared"
			constraints	<- reqs' |>  first (\free -> L.lookup free indices ? errMsg free) |> onFirst & sequence  :: Exc [(Int, RType)]
			let constraints'= constraints & merge & M.fromList
			return ((fqn, n), TypeInfo frees constraints')
