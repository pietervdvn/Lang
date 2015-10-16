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




data Typetable	= Typetable (Map TypeID TypeInfo) deriving (Show, Eq)



data TypeInfo	= TypeInfo {	kind		:: Kind,
				frees		:: [Name], {-The names of the frees are kept merely for the docstrings-}
				constraints	:: Map Int [RType],
				 {- e.g. type Dict (k:Eq) v	= ...
						means we will get {0 --> [Eq]}.
						Constraints can be propagated in a hidden way:
						type X a	= X (Dict a Int)
						implies a should be Eq too.
					-}
				requirements	:: [TypeConstraint],
				{- 	cat A (a:Eq)
						...
					cat B : A Bool
						...

					This means that B is an A of Bool and requires that 'Bool' is an 'Eq'
					This needs to be checked when the full type table is built
					-}
				supertypes	:: Map RType [TypeConstraint]
				{- Dict of supertypes. The keys represent which supertypes this type has (in a normalized form),
					the elements are needed to check if constraints are met
				-}
				} deriving (Show, Eq)

addConstraints		:: [(Int, [RType])] -> TypeInfo -> TypeInfo
addConstraints constr ti
	= let	constr'	= constr & unmerge & merge & M.fromList in
		ti {constraints = M.unionWith (\c c' -> nub (c++c')) constr' $ constraints ti}


addConstraintsWith	:: [(Int, Name)] -> [(Name, [RType])] -> TypeInfo -> TypeInfo
addConstraintsWith mapping constr
	= let	constr'	= buildMapping mapping constr in
		addConstraints constr'

addRequirements		:: [(RType, RType)] -> TypeInfo -> TypeInfo
addRequirements reqs	 ti
	= ti {requirements = nub ((reqs |> uncurry SubTypeConstr) ++ requirements ti)}


buildMapping	:: Eq b => [(a, b)] -> [(b, c)] -> [(a,c)]
buildMapping start end
	= do	(a,b)	<- start
		(b',c)	<- end
		if b == b' then	[(a,c)] else []


data TypeConstraint	= SubTypeConstr RType RType
	deriving (Eq)


instance Show TypeConstraint where
	show	= stc


stc (SubTypeConstr sub super)
	= "Constraint '"++show sub++"' is a '"++show super++"'"


isTrivialConstraint	:: TypeConstraint -> Bool
isTrivialConstraint (SubTypeConstr t0 t1)
			= t0 == t1

instance Documentable Typetable where
	toDocument (Typetable dict)
		= let	docs	= dict & M.toList |> uncurry typeinfo2doc in
			(doc "Typetable" "" $ Base "hi", docs)



typeinfo2doc	:: TypeID -> TypeInfo -> Doc
typeinfo2doc (fqn, nm) ti
		= let 	frees'	= frees ti & (`zip` [0..]) ||>> (\i -> M.findWithDefault [] i $ constraints ti) :: [(Name, [RType])]
			defFrees	= take (length $ frees ti) defaultFreeNames
			frees''	= frees' |||>>> show ||>> commas ||>> when ":" |> (\(nm, reqs) -> nm ++ reqs ) |> code & Mu.Seq
			constrs	= constraints ti & toList |> (\(i,c) -> [code $ defFrees !! i, code $ show c])
			rows	= supertypes ti ||>> (\(SubTypeConstr sub super) -> code (show sub) +++ Base " : " +++ code (show super))
					|> (\constrs -> Mu.Seq $ (if length constrs == 1 then id else (|> Parag)) constrs )
					& M.toList
					|> (\(super, constraints) -> [code $ show super, constraints])	:: [[MarkUp]]
			supers	= table ["Type param", "Constraints"] constrs +++ table ["Supertype","Constraints"] rows
			reqs	= requirements ti |> (\(SubTypeConstr sub super) -> [code $ show sub, code $ show super])
			in
			doc ("Modules/"++show fqn ++"/Overview of "++nm) ("All we know about "++show fqn++"."++nm) $ Titling (code nm +++ frees'') $
				Base "Kind: "+++ code (show $ kind ti) +++
				titling "Requirements" (table ["Type","needs this supertype"] reqs) +++
				Titling (Base "Supertypes of "+++code nm+++ (defFrees |> code & Mu.Seq) ) supers
