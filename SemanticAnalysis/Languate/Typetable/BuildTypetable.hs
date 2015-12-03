module Languate.Typetable.BuildTypetable where


import StdDef
import HumanUtils
import Exceptions
import Languate.CheckUtils

import Languate.Package
import Languate.AST hiding (frees)
import Languate.TAST
import Languate.TypeConstraint.Def
import Languate.TypeConstraint.Utils


import Languate.FQN

import Languate.Typetable.TypetableDef
import Languate.Typetable.TypeLookupTable
import Languate.Typetable.ModuleTraverser
import Languate.Typetable.PropagateImplicitConstraints
import Languate.Typetable.PropagateSupertypeConstraints
import Languate.Typetable.PropagateSupertypes

import Languate.Typetable.ValidateTypeStatements

import Control.Monad

import Data.Set as S
import Data.Map as M
import Data.List as L
import Data.Maybe

import Graphs.DirectedGraph
import Graphs.ExportCalculator (calculateExports, calculateImports)
import Graphs.SearchCycles

import Control.Arrow hiding ((+++))

buildTypetables	:: Package -> Map FQN TypeLookupTable -> Map FQN Module -> Exc (Map FQN Typetable)
buildTypetables p tlts mods
	= do	let globalTLT	= M.unions $ M.elems tlts
		-- first some stupid checks on all the mods
		inside "While doing the pre-checks" $ dictMapM (validateTypeSTMs tlts) mods
		-- builds a dict {FQN --> type related statements}
		let typeStms	= mapWithKey typeStatements mods |> S.fromList
		-- we run those statements through an export calculator
		-- this way, each module sees all the type related statements locally and builds the typetable based on that
		let impGraph	= importGraph p
		let expGraph	= invertDict impGraph
		let fetch fqn	= M.findWithDefault S.empty fqn typeStms
		-- when we would add "import M hiding (instance X is Y)" we would filter this out with this function
		let filterSupertypes _ _	= True
		let exports	= calculateExports impGraph expGraph fetch filterSupertypes
		let known	= calculateImports impGraph fetch exports	:: Map FQN (Set (TypeSTM, FQN))
		let known'	= known |> S.toList ||>> fst |> nub
		dictMapM (buildTypetable globalTLT) known'



{- builds the 'defined' type table from the code.
	-> Direct constraints on the free types are loaded

	The known table is needed for the kinds kinds
-}
buildTypetable	:: TypeLookupTable -> FQN -> TypeSTMs -> Exc Typetable
buildTypetable tlt fqn stms
	= inside ("While building the local type info in module "++show fqn) $
	  do	-- first build the locally known values
		let locDecl	= locallyDeclared stms 	:: [((FQN, Name), [Name], [TypeRequirement])]
		-- now we get all defined supertypes (including synonyms)
		superDecls	<- stms |> declaredSuperType tlt & sequence |> concat
					:: Exc [(RType, [Name], RType, [TypeConstraint])]
		{- we now build the supertype table for locally declared values.
			It picks out of 'superDecls' the supertypes it needs;
		-}
		typeInfos	<- locDecl |+> buildTypeInfo tlt superDecls fqn |> M.fromList |> Typetable
		-- now propagate existence constraints
		tt		<- propagateImplicitConstraints tlt stms typeInfos |> fst
		-- we calculate the kinds of the types. The basic kinds are already there, but special cases are not handled yet
		-- e.g. StateT :: * -> (* -> *) -> * -> *
		-- this is done using the kinds of the constraints
		kindedtt	<- buildKinds tt
		constrComplete	<- addSuperConstraints tlt stms kindedtt
		-- then we calculate the 'transitive closure' of the supertype relationship
		-- for non mathematicians: X is a Y; Y is a Z => X is a Z
		superComplete	<- propagateSupertypes constrComplete
		-- we pass over all type info, to check constraints on existence
		checkTT superComplete
		-- we still have to add the any-type as supertype to each TI
		let superCompleteAny = forTi addAnySupertype superComplete
		cleaned	<- forTiM cleanTI superCompleteAny
		return cleaned

addAnySupertype	:: Typetable -> TypeID -> TypeInfo -> TypeInfo
addAnySupertype _ (fqn, name) ti
 | supertypes ti & M.keys |> isNormal & and
 	= ti {supertypes = supertypes ti & M.insert anyType []}
 | otherwise
 	= ti	-- means some super type is e.g. "a -> a", a curry. These are not subclasses of any!




cleanTI		:: Typetable -> TypeID -> TypeInfo -> Exc TypeInfo
cleanTI tt _ ti	= do	let stts	= supertypes ti	& M.toList :: [(RType, [TypeConstraint])]
			-- existance constraints on frees
			let metConstraintsFrees	= constraints ti & M.toList
							|> first (\i -> defaultFreeNames !! i)
							|> first RFree & unmerge
							|> (\(a, super) -> SubTypeConstr a super)
			-- constraints on frees + other requirements
			let metConstraints	= (metConstraintsFrees ++ requirements ti) & S.fromList	:: Set TypeConstraint
			cleanedStts	<- stts |+> cleanSTT tt metConstraints
						|> M.fromList	:: Exc (Map RType [TypeConstraint])
			return $ ti{supertypes = cleanedStts}

cleanSTT	:: Typetable -> Set TypeConstraint -> (RType, [TypeConstraint]) -> Exc (RType, [TypeConstraint])
cleanSTT tt met (rt, constrs)
	= do	statusses	<- constrs |+> isConstraintMet' tt met
		let unmet	= zip statusses constrs & L.filter (not . fst) |> snd
		let trivial	= unmet & L.filter isTrivialConstraint
		return (rt, unmet & L.filter (not . isTrivialConstraint))



buildKinds	:: Typetable -> Exc Typetable
buildKinds tt
	= whileChanged _buildKinds tt |> fst


_buildKinds	:: Typetable -> Exc (Typetable, Changed)
_buildKinds tt@(Typetable conts)
	= do	conts'	<- dictMapM (buildKindFor tt) conts
		let changed = conts' & M.elems |> snd & or
		return (conts' |> fst & Typetable, changed)

buildKindFor	:: Typetable -> TypeID -> TypeInfo -> Exc (TypeInfo, Changed)
buildKindFor tt tid ti
	= do	let oldKind	= kind ti
		-- we build the kinds (simply) based on the kinds of the constraints
		kindArgs	<- buildFreeKinds tt ti ||>> snd
		let newKind	= buildKind kindArgs
		return (ti {kind = newKind}, oldKind /= newKind)

-- builds what kind each free type variable has, for given TI
buildFreeKinds	:: Typetable -> TypeInfo -> Exc [(Name, Kind)]
buildFreeKinds tt ti
	= do	let nrOfFrees	= length (frees ti)
		-- we take a representative out of the constraints
		let constraintExamples
			= [0.. nrOfFrees -1] |> flip (M.findWithDefault [anyType]) (constraints ti)
				|> sort |> head & zip defaultFreeNames	-- we sort, as to put RFrees to the end -> might prevent cycles
				:: [(Name, RType)]
		let cyclesDG	= constraintExamples & L.filter (isRFree . snd) ||>> (\(RFree a) -> S.singleton a) & M.fromList
					:: DirectedGraph Name
		let asFreeName	= zip defaultFreeNames (frees ti)	:: [(Name, Name)]
		let cycles	= cleanCycles cyclesDG ||>> (\n -> L.lookup n asFreeName & fromMaybe n) |> commas
		haltIf (not $ L.null cycles) $ "The free type variables contain cycles, what makes calculating the kind impossible. Cycles are:"++
						indent ("\n"++unlines cycles)
		let kindOf name
			= do	rt	<- L.lookup name constraintExamples ? ("The free type variable "++show name++" was not defined")
				simpleKindOf tt kindOf rt
 		kindArgs	<- take nrOfFrees defaultFreeNames |+> kindOf	:: Exc [Kind]
		return $ zip defaultFreeNames kindArgs








checkTT		:: Typetable -> Check
checkTT tt@(Typetable conts)
	= do 	conts & M.toList |+> uncurry (checkTi tt)
		pass



checkTi		:: Typetable -> TypeID -> TypeInfo -> Check
checkTi tt tid ti
	= inside ("While checking the type constraints on "++show tid) $
	  inside ("This type has a supertype which imposes restrictions. Not all restrictions are met, thus this type can not exist") $
	  do	let reqs	= requirements ti
		unmet		<- reqs & filterM (fmap not . isConstraintMet tt )
		assert (L.null unmet) $ "Unmet constraints are: "++show unmet
		let freeKinds	= kind ti & kindArgs & zip defaultFreeNames
		dictMapM (checkSameKind tt freeKinds ti) (constraints ti)
		requirements ti |+> (inside "In the type existence requirements" . checkTypeConstraint tt freeKinds)
		dictMapM (checkSupertype tt freeKinds ti) (supertypes ti)
		pass


checkSupertype	:: Typetable -> [(Name, Kind)] -> TypeInfo -> RType -> [TypeConstraint] -> Check
checkSupertype tt ctx ti super reqs
	= inside ("On the type requirements for the supertype "++show super) $
	  do	reqs |+> checkTypeConstraint tt ctx
		pass


checkTypeConstraint	:: Typetable -> [(Name, Kind)] -> TypeConstraint -> Check
checkTypeConstraint tt ctx (SubTypeConstr t0 t1)
	= do	k0	<- kindOf tt ctx t0
		k1	<- kindOf tt ctx t1
		let st t k	= "\n"++show t ++ " :: "++show k
		assert (k0 == k1) ("Types in a typeconstraint should have the same kind."++
			indent (st t0 k0 ++ st t1 k1 ))
checkTypeConstraint tt ctx (Choose a b)
	= do	checkTypeConstraint tt ctx a
		checkTypeConstraint tt ctx b
checkTypeConstraint tt ctx (All conss)
	= do	conss |+> checkTypeConstraint tt ctx
		pass


checkSameKind	:: Typetable -> [(Name, Kind)] -> TypeInfo -> Int -> [RType] -> Check
checkSameKind _ _ _ _ []	= pass
checkSameKind  tt ctx ti i rtps
	= inside ("In the type constraint of "++ (frees ti !! i)) $
	  do	lst@(kind:kinds)	<- rtps |+> kindOf tt ctx
		let msg = zip rtps lst |> (\(rt, k) -> show rt ++" :: "++show k) & unlines & ("\n"++)
		assert (all (== kind) kinds) $ "Types in a constraint should all have the same kinds. Given types are:"++indent msg





-- Builds the type info about the given type in the tuple. The fqn is the module for which we build the typetable, the second is where it was defined
buildTypeInfo	:: TypeLookupTable -> [(RType, [Name], RType, [TypeConstraint])] -> FQN
				-> ((FQN, Name), [Name], [(Name, Type)]) -> Exc (TypeID, TypeInfo)
buildTypeInfo tlt superDecls fqn ((definedIn, name), frees, reqs)
	= inside ("While building type info about "++show definedIn++"."++show name) $
	  do	tid		<- resolveTypeID tlt (modulePath definedIn, name)-- TODO resolveTypeID tlt n
		let kind	= L.foldl (\kind free -> KindCurry Kind kind) Kind frees
		-- about the free type names
		let indices	= zip frees [0..]	:: [(Name, Int)]
		reqs'		<- reqs |+> (\(nm, t) -> do rt<-resolveType tlt t;return (nm, rt) )	:: Exc [(Name, RType)]
		reqs' |> snd |+> validateFrees frees

		-- about the constraints
		constraints	<- reqs' |+>  onFirst (fetch indices)	:: Exc [(Int, RType)]
		-- mapping to normalize the constraints
		let mapping	= zip frees defaultFreeNames ||>> RFree
		constraints'	<- constraints |+> onSecond (subs mapping) |> merge
					|> M.fromList			:: Exc (Map Int [RType])
		-- about the supertypes
		let isTid t	= getBaseTID t |> (tid ==) & fromMaybe False	:: Bool
		superTypes	<- superDecls & L.filter (isTid . fst4) |+> canonicalSuperDecl
					:: Exc [(RType, [Name], RType, [TypeConstraint])]
		let duplicateSupers	= superTypes |> (\(_,_,super,_) -> super) & dubbles
		assert (L.null duplicateSupers) $
			"Multiple declarations of supertypes. "++show tid++
			" has multiple instance declarations of "++
			commas (duplicateSupers |> show)
		supers		<- superTypes |+> buildSTTEntry |> M.fromList
		let superOrigins	= supers |> const fqn
		return (tid, TypeInfo kind frees constraints' [] supers superOrigins fqn)


{-Converts the super type and frees into default free names-}
canonicalSuperDecl	:: (RType, [Name], RType, [TypeConstraint]) -> Exc (RType, [Name], RType, [TypeConstraint])
canonicalSuperDecl (sub, frees, super, constraints)
		= do	let allFrees	= frees ++ freesInRT sub ++ freesInRT super	-- the constraints should not contain new frees
			-- conversion mapping to default names
			let canonFree	= zip (nub allFrees) defaultFreeNames
			let mapping	= canonFree ||>> RFree

			-- default frees we'll actually use
			let frees'	= canonFree |> snd
			sub'		<- subs mapping sub
			super'		<- subs mapping super
			constraints' 	<- constraints |+> subsConstraint mapping
			return (sub', frees', super', constraints')





buildSTTEntry	:: (RType, [Name], RType, [TypeConstraint]) -> Exc (RType, [TypeConstraint])
buildSTTEntry (sub, frees, super, constraints)
		= do	validateFrees frees sub
			return (super, constraints)

fetch indices free	= L.lookup free indices ? errMsg free
			where errMsg free	= "The free type variable "++free++" was not declared"
