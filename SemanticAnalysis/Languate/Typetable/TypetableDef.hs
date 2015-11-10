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



type TypeSTMs	= [([Name], Statement)]

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
				supertypes	:: Map RType [TypeConstraint],
				supertypeOrigin	:: Map RType FQN,
				{- Dict of supertypes. The keys represent which supertypes this type has (in a normalized form),
					the elements are needed to check if constraints are met
				-}
				origin		:: FQN	{-Where it is defined-}
				} deriving (Show, Eq, Ord)

addConstraints		:: [(Int, [RType])] -> TypeInfo -> TypeInfo
addConstraints constr ti
	= let	constr'	= constr & unmerge & merge & M.fromList in
		-- the merge function should be stable! e.g. [1,2,3] (old) ++ [1] (new) & nub -> [1,2,3]
		-- but [3] (new) ++ [1,2,3] (old) --> [3,1,2], which trips the (==) and marks typeinfo as changed (even if it hasn't)
		ti {constraints = M.unionWith (\c c' -> nub (c'++c)) constr' $ constraints ti}


addConstraintsWith	:: [(Int, Name)] -> [(Name, [RType])] -> TypeInfo -> TypeInfo
addConstraintsWith mapping constr
	= let	constr'	= buildMapping mapping constr in
		addConstraints constr'

addRequirements		:: [(RType, RType)] -> TypeInfo -> TypeInfo
addRequirements reqs	 ti
	= ti {requirements = nub (requirements ti ++ (reqs |> uncurry SubTypeConstr))}
		-- the merge function should be stable! e.g. [1,2,3] (old) ++ [1] (new) & nub -> [1,2,3]
		-- but [3] (new) ++ [1,2,3] (old) --> [3,1,2], which trips the (==) and marks typeinfo as changed (even if it hasn't)



buildMapping	:: Eq b => [(a, b)] -> [(b, c)] -> [(a,c)]
buildMapping start end
	= do	(a,b)	<- start
		(b',c)	<- end
		[(a,c) | b == b']


kindOf		:: Typetable -> [(Name, Kind)] -> RType -> Exc Kind
kindOf _ ctx (RFree a)
	= L.lookup a ctx ? ("The free type variable "++show a++" was not declared within this context")
kindOf tt _  t@(RNormal _ _)
	= getTi tt t |> kind
kindOf tt ctx (RApplied baseT argT)
	= do	baseKind	<- kindOf tt ctx baseT
		argKind		<- kindOf tt ctx argT
		case baseKind of
			Kind	-> halt ("The type "++show baseT++" was applied to too many arguments, namely "++show argT)
			(KindCurry arg result)
				-> do	assert (arg == argKind) ("The kind of "++show argT++", namely "++show argKind++" does not match the expected kind "++show arg++", imposed by "++show baseT)
					return result
kindOf tt ctx (RCurry t0 t1)
	= do	let chck = assertNormalKind tt ctx (\t k -> "A type within a curry should have a kind '*', but "++show t++" has the kind "++show k)
		chck t0
		chck t1
		return Kind

assertNormalKind	:: Typetable -> [(Name, Kind)] -> (RType -> Kind -> String) -> RType -> Exc ()
assertNormalKind tt ctx str t
	= do	k	<- kindOf tt ctx t
		assert (Kind == k) $ (str t k)


-- gets the type info for the given type, as seen in the known table
getTi'		:: Typetable -> TypeID -> Exc TypeInfo
getTi' (Typetable conts) tid
		= conts & M.lookup tid ? ("No type info found for "++show tid++", weird")

getTi		:: Typetable -> RType -> Exc TypeInfo
getTi tt rt	= do	tid	<- getTid rt
			getTi' tt tid

getTid		:: RType -> Exc TypeID
getTid rt	= getBaseTID rt ? ("No tid found for "++show rt)


supertypesOf'	:: Typetable -> TypeID -> Exc [(RType, [TypeConstraint])]
supertypesOf' tt tid
	= do	ti	<- getTi' tt tid
		supertypes ti & M.toList & return


-- supertypes of RT, expressed in the free types, with it's requirements
supertypesOf	:: Typetable -> RType -> Exc [(RType, [TypeConstraint])]
supertypesOf tt rt
	= do	{- a0 --> type -}
		let mapping	= appliedTypes rt & zip defaultFreeNames
		tid		<- getBaseTID rt ? ("No type id could be found for "++show rt)
		superTypes	<- supertypesOf' tt tid
		superTypes'	<- superTypes |+> onFirst (subs mapping)
		superTypes' |+> onSecond (|+> subsConstraint mapping)



data TypeConstraint	= SubTypeConstr RType RType
	deriving (Eq, Ord)


subsConstraint	:: [(Name, RType)] -> TypeConstraint -> Exc TypeConstraint
subsConstraint mapping (SubTypeConstr a b)
	= do	a'	<- subs mapping a
		b'	<- subs mapping b
		return (SubTypeConstr a' b')


isConstraintMet' :: Typetable -> [TypeConstraint] -> TypeConstraint -> Exc Bool
isConstraintMet' tt metConstraints constraint@(SubTypeConstr sub super)
 | constraint `elem` metConstraints 	= return True
 | otherwise	= do	(supers, constraints)	<- supertypesOf tt sub |> unzip ||>> concat
			metConstrs	<- constraints |+> isConstraintMet' tt (metConstraints++[constraint]) |> and
			return (metConstrs && (super `elem` supers))

isConstraintMet	:: Typetable -> TypeConstraint -> Exc Bool
isConstraintMet tt
	= isConstraintMet' tt []


instance Show TypeConstraint where
	show	= stc


stc (SubTypeConstr sub super)
	= "Constraint '"++show sub++"' is a '"++show super++"'"


isTrivialConstraint	:: TypeConstraint -> Bool
isTrivialConstraint (SubTypeConstr t0 t1)
			= t0 == t1

typetable2doc	:: (FQN -> String) -> FQN -> Typetable -> (Doc, [Doc])
typetable2doc path fqnView (Typetable dict)
		= let	docs	= dict & M.toList |> uncurry (typeinfo2doc path fqnView) in
			(doc (path fqnView++"Typetable of "++show fqnView) ("All typeinfo that is known within the module "++show fqnView) $
				docs |> title |> Embed & Mu.Seq, docs)



typeinfo2doc	:: (FQN -> String) -> FQN -> TypeID -> TypeInfo -> Doc
typeinfo2doc path fqnView (fqnDef, nm) ti
		= let 	frees'	= frees ti & (`zip` [0..]) ||>> (\i -> M.findWithDefault [] i $ constraints ti) :: [(Name, [RType])]
			defFrees	= take (length $ frees ti) defaultFreeNames
			frees''	= frees' |||>>> show ||>> commas ||>> when ":" |> uncurry (++) |> code & Mu.Seq
			rows	= supertypes ti ||>> (\(SubTypeConstr sub super) -> code (show sub) +++ Base " : " +++ code (show super))
					|> (\constrs -> Mu.Seq $ (if length constrs == 1 then id else (|> Parag)) constrs )
					& M.toList
					|> (\(super, constraints) -> [code $ show super, constraints])	:: [[MarkUp]]
			supers	= table ["Supertype","Constraints"] rows
			nrOfSupers	= rows & L.length
			supers'	= if nrOfSupers > 0 then Titling (Base "Supertypes of "+++code nm+++ (defFrees |> code & Mu.Seq) ) supers
						else parag "This type has no supertypes"
			constrs	= constraints ti & toList |> (\(i,c) -> [code $ defFrees !! i, c |> show |> code & Mu.Seq])
					& table ["Type param", "Constraints"]
			nrOfConstr
				= constraints ti & M.size
			constrs'= if nrOfConstr > 0 then Parag (Base ("Typeconstraints on the free type variables."++
					" This means that a specific type should be used as corresponding type argument")+++constrs) else Base ""
			reqs	= requirements ti |> (\(SubTypeConstr sub super) -> [code $ show sub, code $ show super])
					& table ["Type","needs this supertype"]
			nrOfReqs
				= requirements ti & L.length
			reqs'	= if nrOfReqs > 0 then Parag (Base "As a type "+++code "T" +++
					 Base "is used in a type argument "+++code "a"+++Base ("of a supertype, "++
					"it might be that the constraint ")+++code "C"+++Base "is needed on this type "+++code "T"+++reqs)
					else Base ""
			reqsConstr	= if nrOfConstr + nrOfReqs > 0 then titling "Constraints and requirements" (reqs' +++ constrs') else Base ""
			in
			doc (path fqnView ++"Overview of "++nm) ("All we know about "++show fqnDef++"."++nm++" as seen within "++show fqnView) $ Titling (code nm +++ frees'') $
				Base "Kind: "+++ code (show $ kind ti) +++
				Base "Defined in: "+++ code (show $ origin ti)+++
				reqsConstr +++ supers'
