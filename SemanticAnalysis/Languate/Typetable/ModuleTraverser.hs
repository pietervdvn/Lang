module Languate.Typetable.ModuleTraverser
	(locallyDeclared, declaredType, declaredSuperType
	, constraintAdoptions, typeSynonyms, pushedSupers
	, typeStatements) where

{-
Multiple functions to traverse the statements, in search for type declaration stuff
-}

import StdDef
import Exceptions
import Languate.CheckUtils

import Languate.Typetable.TypeLookupTable.TypeLookupTableDef
import Languate.Typetable.TypeLookupTable.TypeLookupTableUtils

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L
import Data.Set hiding (map, filter)
import Data.Map hiding (map, filter, mapMaybe)
import Data.Maybe
import Data.Tuple

import Languate.AST as AST
import Languate.TAST
import Languate.FQN
import Languate.Package



-- All locally declared types
locallyDeclared	:: [([Name], Statement)] -> [(([Name], Name), [Name], [TypeRequirement])]
locallyDeclared statements
		=  statements |> declaredType & catMaybes
			|> (\(a,b,c,d) -> ((a,b),c,d))

-- filters all statements which have to do something with types
typeStatements	:: FQN -> Module -> [([Name], Statement)]
typeStatements fqn mod
	= let origin	= modulePath fqn in
		mod & statements & L.filter isTypeRelated & zip (repeat origin)

isTypeRelated	:: Statement -> Bool
isTypeRelated (ADTDefStm{})
		= True
isTypeRelated (SubDefStm{})
		= True
isTypeRelated (ClassDefStm{})
		= True
isTypeRelated (InstanceStm{})
		= True
isTypeRelated _	= False



declaredType :: ([Name], Statement) -> Maybe ([Name], Name, [Name], [TypeRequirement])
declaredType (origin, ADTDefStm (ADTDef name frees reqs _ _))
		= Just (origin, name, frees, reqs)
declaredType (origin, SubDefStm (SubDef name _ frees _ reqs))
		= Just (origin, name, frees, reqs)
declaredType (origin, ClassDefStm classDef)
		= Just (origin, name classDef, frees classDef, classReqs classDef)
declaredType _	= Nothing



declaredSuperType	:: TypeLookupTable -> ([Name], Statement) -> Exc [(RType, [Name], CType)]
declaredSuperType tlt (origin, ADTDefStm (ADTDef nm frees reqs sums adopts))
	= do	-- the type being declared here is the supertype of the adopted types...
		super	<- resolveTypePath tlt (origin, nm)
		-- ... applied on the frees of course!
		let super'	= applyTypeArgs super (frees |> RFree)
		adopts'	<- resolveTypes tlt adopts
		reqs'	<- resolveReqs tlt reqs
		adopts' |> (\typ -> (typ, frees, (super', reqs'))) & return
		{- whenever there are no constructors (and only a single adopted type),
			we declare a synonym; this case is handles elsewhere as to prevent supertype loops -}
declaredSuperType tlt (_,InstanceStm (Instance typePath frees super reqs))
	= do	typ	<- resolveTypePath tlt typePath
		let typ'= applyTypeArgs typ (frees |> RFree)
		super'	<- resolveType tlt super
		reqs'	<- resolveReqs tlt reqs
		(typ', frees, (super', reqs')) & return & return
declaredSuperType tlt (origin, SubDefStm (SubDef name _ frees supers reqs))
	= do	typ	<- resolveTypePath tlt (origin, name)
		supers'	<- resolveTypes tlt supers
		reqs'	<- resolveReqs tlt reqs
		supers' |> (\super -> (typ, frees, (super, reqs'))) & return
declaredSuperType tlt (origin, ClassDefStm cd)
	= do	typ	<- (origin, AST.name cd) & resolveTypePath tlt
		supers	<- subclassFrom cd & resolveTypes tlt
		reqs	<- classReqs cd & resolveReqs tlt
		let frees	=  AST.frees cd
		supers |> (\super -> (typ, frees, (super, reqs))) & return
declaredSuperType _ _	= return []


{-

cat A0 a:A1 a implies that A0 has the same constraints on 'a' as A1.
This function gives exactly this relation:
[A0 a, A1 a]

For CATEGORY- and TYPE-DECLARATIONS only, as the constraints imposed here are constraints to exist, not to have a certain supertype

-}
constraintAdoptions	:: TypeLookupTable -> ([Name], Statement) -> Exc [(RType, RType)]
constraintAdoptions tlt stm@(_, ClassDefStm _)
	-- ""cat X a:Y a"" : X (rt) only exists if the constraints on super are met
	= declaredSuperType tlt stm ||>> (\(rt, frees, (super, constraints)) -> (applyTypeArgs rt (frees |> RFree), super))
constraintAdoptions tlt stm@(_, ADTDefStm _)
	-- ""type X a={Constr} + Y a"": X (rt) only exists if the constraints on super are met
	= declaredSuperType tlt stm ||>> (\(super, frees, (rt, constraints)) -> (applyTypeArgs rt (frees |> RFree), super))
constraintAdoptions _ _	= return []


{-
Returns the supertypes which are **pushed** upon types by a type declaration, e.g.

type List a = ...
type Char = ...
type String = List Char

This last declaration 'pushes' the supertype 'String' onto 'List', but with requirements that a0 is a Char...

[(subForm, superForm)]

-}
pushedSupers		:: TypeLookupTable -> ([Name], Statement) -> Exc [(RType, (RType,[Name]))]
pushedSupers tlt (origin, ADTDefStm (ADTDef nm frees _ _ adopted))
	= do	super		<- resolveTypePath tlt (origin,nm)	-- type that is pushed on ...
		subForms	<- resolveTypes tlt adopted -- ... these fellows
		[((super,frees), subForms)] & unmerge |> swap & return
pushedSupers _ _
	= return []

{-
Returns the supertypes which are **pulled** upon types by a type declaration.
-}
typeSynonyms		:: TypeLookupTable -> ([Name], Statement) -> Exc [(RType, [Name], RType)]
typeSynonyms tlt (origin, ADTDefStm (ADTDef nm frees _ [] [synonym]))
	-- reqs are ignored, these are handled in other fases
	= do	typ	<- resolveTypePath tlt (origin,nm)
		syn'	<- resolveType tlt synonym -- aka super
		return [(typ, frees, syn')]
typeSynonyms _ _
	= return []
