module Languate.Typetable.ValidateTypeStatements where

{--

All kind of type statement checks:
	- instance X is Y a -> undefined a
--}
import StdDef
import HumanUtils
import Exceptions
import Languate.CheckUtils

import Languate.AST
import Languate.TAST
import Languate.FQN

import Languate.Typetable.ModuleTraverser
import Languate.Typetable.TypeLookupTable
import Languate.TypeConstraint


import Data.Maybe
import Data.Map

validateTypeSTMs	:: Map FQN TypeLookupTable -> FQN -> Module -> Check
validateTypeSTMs tlts fqn mod
	= inside ("In the module "++show fqn)$
	  do	tlt	<- getTLT tlts fqn
		mod & statements' |+> validateTypeSTM tlt fqn >> pass

validateTypeSTM	:: TypeLookupTable -> FQN -> (Statement, Coor) -> Check
validateTypeSTM tlt fqn (stm, (l,_))
	= inside ("On line "++show l) $
	  do	let stm'	= (fqn, stm)
		declaredType stm' & maybeToList
				|+> validateDeclaration tlt
		supers		<- declaredSuperType tlt stm'
		supers |+> validateSuperDeclaration tlt
		pass


validateSuperDeclaration	:: TypeLookupTable -> (RType, [Name], RType, [TypeConstraint]) -> Check
validateSuperDeclaration tlt (sub, frees, super, constrs)
	= inside ("In the supertype declaration of "++show sub) $
	  do	validateFrees frees sub
		validateConstraintFrees frees super constrs




validateDeclaration	:: TypeLookupTable -> (FQN, Name, [Name], [TypeRequirement]) -> Check
validateDeclaration tlt (origin, declaredType, frees, treqs)
	= inside ("In the declaration of "++show origin++"."++declaredType) $
		mapM_ (validateReq tlt frees) treqs

validateConstraintFrees	:: [Name] -> RType -> [TypeConstraint] -> Check
validateConstraintFrees frees rt constrs
	= do	validateFrees frees rt
		let leaves	= constrs >>= allConstrs
		leaves |+> (\(SubTypeConstr sub super) -> validateFrees frees sub >> validateFrees frees super)
		pass


validateReqsFrees	:: [Name] -> RTypeReq -> Check
validateReqsFrees frees vals
	= do	vals |> fst |+> (\nm -> assert (nm `elem` frees) $ "The free type variable "++show nm++" was not declared")
		(vals >>= snd) |+> validateFrees frees
		pass

validateReq	:: TypeLookupTable -> [Name] -> TypeRequirement -> Check
validateReq tlt frees (name, typ)
	= inside ("In the type requirement of "++show name)
	  (resolveType tlt typ >>= validateFrees frees)



--validates that no unknown frees are used in rt
validateFrees		:: [Name] -> RType -> Check
validateFrees frees rt	= do	let foundFrees 	= freesInRT rt
				mapM_ (\a -> assert (a `elem` frees) $ "The free type variable "++show a++" was not declared") foundFrees
