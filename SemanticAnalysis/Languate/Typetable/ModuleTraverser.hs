module Languate.Typetable.ModuleTraverser
	(locallyDeclared, declaredType, declaredSuperType
	, constraintAdoptions, pushedSupers
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
		mod & statements & L.filter isTypeRelated
			|> rewriteTypeStm origin	-- rewrite, to prevent ambiguities, e.g. types with the same name
			& zip (repeat origin)

rewriteTypeStm	:: [Name] -> Statement -> Statement
rewriteTypeStm origin
	= id  -- TODO

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


{-Gives a super type relationship
	[(subtype, frees, supertype+reqs)]
-}
declaredSuperType	:: TypeLookupTable -> ([Name], Statement) -> Exc [(RType, [Name], CType)]
declaredSuperType tlt (origin, ADTDefStm adtDef@(ADTDef nm frees reqs sums adopts))
	= inside ("While building the supertype relation of "++show nm++" "++show frees) $
	  do	reqs'	<- resolveReqs tlt reqs
		-- the type being declared here is the supertype of the adopted types...
		declaredType	<- resolveTypePath tlt (origin, nm)
		-- ... applied on the frees of course!
		let declaredType'	= applyTypeArgs declaredType (frees |> RFree)
		adopts'	<- adopts & resolveTypes tlt
		-- an adopted type has this type as supertype, if this adopted type is not a conjunction of types
		-- TODO:
		-- in case of conjuctions is the adopted type this type too if it is all the other types of the conjunction
		let super'	= declaredType'	-- declared type = supertype of the adopts
		let normalSupers	= adopts' & L.filter isConjFree |> (\typ -> (typ, frees, (super', reqs')))
		-- this type is also the adopted type, if there is only one adopted type
		-- and no constructors are defined
		let allAdopts	= adopts' >>= rtopLevelConj
		let synonymSupers	= allAdopts |> (\adoptThusSuper -> (declaredType', frees, (adoptThusSuper, reqs')) )
		let synonymSupers'	= if isSynonym adtDef then synonymSupers else []
		return (normalSupers ++ synonymSupers')
declaredSuperType tlt (_,InstanceStm (Instance typePath frees super reqs))
	= do	typ	<- resolveTypePath tlt typePath
		let typ'= applyTypeArgs typ (frees |> RFree)
		supers	<- super & topLevelConj & resolveTypes tlt
		reqs'	<- resolveReqs tlt reqs
		let declared
			= supers |> (\super' -> (typ', frees, (super', reqs')))
		return declared
declaredSuperType tlt (origin, SubDefStm (SubDef name _ frees supers reqs))
	= do	typ	<- resolveTypePath tlt (origin, name)
		supers'	<- resolveTypes tlt (supers >>= topLevelConj)
		reqs'	<- resolveReqs tlt reqs
		supers' |> (\super -> (typ, frees, (super, reqs'))) & return
declaredSuperType tlt (origin, ClassDefStm cd)
	= do	typ	<- (origin, AST.name cd) & resolveTypePath tlt
		supers	<- (subclassFrom cd >>= topLevelConj) & resolveTypes tlt
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
	= declaredSuperType tlt stm ||>> (\(rt, frees, (super, constraints)) -> (applyTypeArgsFree rt frees, super))
constraintAdoptions tlt stm@(_, ADTDefStm adtDef@(ADTDef _ _ _ _ adopts))
	-- ""type X a = Y a"" is a synonym, therefor only exists if constraints on super are met
	-- ""type X a={Constr} + Y a"": can exists, but then it ain't a Y is constraints are not met
 | isSynonym adtDef
	= do	adopts'	<- (adopts >>= topLevelConj) & resolveTypes tlt
		superRel	<-  declaredSuperType tlt stm ||>> (\(super, frees, (rt, constraints)) -> (rt, super))
		-- adopted types never get any new constraints
		-- we filter out values where the subtype comes from an adopted type
		superRel & L.filter ( (`notElem` adopts') . fst) & return
 | otherwise
 	= return []
constraintAdoptions tlt stm@(_, SubDefStm _)
	-- ""subtype X a = Y a"" X (rt) only exists if the constraints on super are met
	= declaredSuperType tlt stm ||>> (\(rt, frees, (super, constraints)) -> (applyTypeArgsFree rt frees, super))
constraintAdoptions _ _	= return []


isSynonym	:: ADTDef -> Bool
isSynonym (ADTDef _ _ _ sums adopts)
	= L.null sums && length adopts == 1


{-
Returns the supertypes which are **pushed** upon types by a type declaration, e.g.

type List a = ...
type Char = ...
type String = List Char

This last declaration 'pushes' the supertype 'String' onto 'List', but with requirements that a0 is a Char...

[(subForm, superForm)]

This works only if a single type is defined;
type A = C & B
does not imply that C is A.

type A = B + C
however does define that B is an A, just as C is an A
-}

pushedSupers		:: TypeLookupTable -> ([Name], Statement) -> Exc [(RType, (RType,[Name]))]
pushedSupers tlt (origin, ADTDefStm (ADTDef nm frees _ _ adopted))
	= do	super		<- resolveTypePath tlt (origin,nm)	-- type that is pushed on ...
		subForms	<- resolveTypes tlt adopted -- ... these fellows, all the adopted types if they are not conjunctions
						|> L.filter isConjFree
		[((super,frees), subForms)] & unmerge |> swap & return
pushedSupers _ _
	= return []
