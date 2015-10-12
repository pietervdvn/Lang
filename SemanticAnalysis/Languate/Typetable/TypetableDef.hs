module Languate.Typetable.TypetableDef where


import StdDef
import Languate.MarkUp as Mu
import HumanUtils (when, commas, intercal)

import Exceptions
import Languate.CheckUtils

import Languate.AST hiding (frees)
import Languate.TAST
import Languate.FQN

import Languate.Typetable.TypeLookupTable
import Languate.Typetable.ModuleTraverser

import Data.Map as M
import Data.List as L
import Data.Tuple




data Typetable	= Typetable (Map TypeID TypeInfo) deriving (Show)



data TypeInfo	= TypeInfo {	frees		:: [Name], {-The names of the frees are kept merely for the docstrings-}
				constraints	:: Map Int [RType]
				 {- e.g. type Dict (k:Eq) v	= ...
						means we will get {0 --> [Eq]}.
						Constraints can be propagated in a hidden way:
						type X a	= X (Dict a Int)
						implies a should be Eq too.
					-}
				} deriving (Show)





instance Documentable Typetable where
	toDocument (Typetable dict)
		= let	docs	= dict & M.toList |> uncurry typeinfo2doc in
			(doc "Typetable" "" $ Base "hi", docs)



typeinfo2doc	:: TypeID -> TypeInfo -> Doc
typeinfo2doc (fqn, nm) ti
		= let 	frees'	= frees ti & (`zip` [0..]) ||>> (\i -> M.findWithDefault [] i $ constraints ti) :: [(Name, [RType])]
			frees''	= frees' |||>>> show ||>> commas ||>> when ":" |> (\(nm, reqs) -> nm ++ reqs ) |> code & Mu.Seq
			in
			doc ("Modules/"++show fqn ++"/Overview of "++nm) ("All we know about "++show fqn++"."++nm) $ Titling (code nm +++ frees'') $ Base "hi"
