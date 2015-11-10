module Languate.Typetable.BuildTypetable where


import StdDef
import HumanUtils
import Exceptions
import Languate.CheckUtils

import Languate.Package
import Languate.AST hiding (frees)
import Languate.TAST
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
		dictMapM (\fqn mod -> validateTypeSTMs globalTLT fqn mod) mods
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
		let known	= calculateImports impGraph fetch exports	:: Map FQN (Set (([Name], Statement), FQN))
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
		let locDecl	= locallyDeclared stms 	:: [(([Name], Name), [Name], [TypeRequirement])]
		-- now we get all defined supertypes (inclusing synonyms)
		directSupers	<- stms |> declaredSuperType tlt & sequence |> concat
					:: Exc [(RType, [Name], CType)]
		synons	<- stms |+> typeSynonyms tlt |> concat
				||>> (\(rt, frees, super) -> (rt, frees, (super,[])))	:: Exc [(RType, [Name], CType)]
		let superDecls	= directSupers ++ synons
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
		-- and as last, we pass over all type info, to check constraints
		checkTT superComplete
		return superComplete




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
				_simpleKindOf tt kindOf rt
 		kindArgs	<- take nrOfFrees defaultFreeNames |+> kindOf	:: Exc [Kind]
		return $ zip defaultFreeNames kindArgs


buildKind	:: [Kind] -> Kind
buildKind []	= Kind
buildKind (arg:args)
		= let tail	= buildKind args in
			KindCurry arg tail

-- a simple kind calculator, which does not check kinds of arguments (only too much type arguments fails)
_simpleKindOf	:: Typetable -> (Name -> Exc Kind) -> RType -> Exc Kind
_simpleKindOf _ freeKinds (RFree free)
	= freeKinds free
_simpleKindOf tt _ (RNormal fqn n)
	= getTi' tt (fqn, n) |> kind
_simpleKindOf tt freeKinds t@(RApplied base arg)
	= do	baseKind	<- _simpleKindOf tt freeKinds base
		case baseKind of
			Kind	-> halt ("The type "++show base++" is applied to too many arguments")
			(KindCurry _ kind)	-> return kind
_simpleKindOf _ _(RCurry _ _)
	= return Kind





checkTT		:: Typetable -> Check
checkTT tt@(Typetable conts)
	= do 	conts & M.toList |+> uncurry (checkTi tt)
		pass



checkTi		:: Typetable -> TypeID -> TypeInfo -> Check
checkTi tt tid ti
	= inside ("While checking the type constraints on "++show tid) $
	  do	let reqs	= requirements ti
		unmet		<- reqs & filterM (\constr -> isConstraintMet tt constr |> not)
		let msg (SubTypeConstr sub super)
				= show sub ++ " is supposed to have the supertype "++show super
		let msgs	= unmet |> msg & unlines
		assert (L.null unmet) ("Some constraints are not met:\n"++indent msgs)
		let freeKinds	= kind ti & kindArgs & init & zip defaultFreeNames
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


checkSameKind	:: Typetable -> [(Name, Kind)] -> TypeInfo -> Int -> [RType] -> Check
checkSameKind _ _ _ _ []	= pass
checkSameKind  tt ctx ti i rtps
	= inside ("In the type constraint of "++ (frees ti !! i)) $
	  do	lst@(kind:kinds)	<- rtps |+> kindOf tt ctx
		let msg = zip rtps lst |> (\(rt, k) -> show rt ++" :: "++show k) & unlines & ("\n"++)
		assert (all (== kind) kinds) $ "Types in a constraint should all have the same kinds. Given types are:"++indent msg





-- Builds the type info about the given type in the tuple
buildTypeInfo	:: TypeLookupTable -> [(RType, [Name], CType)] -> FQN -> (([Name], Name), [Name], [(Name, Type)]) -> Exc (TypeID, TypeInfo)
buildTypeInfo tlt superDecls fqn (n, frees, reqs)
	= inside ("While building type info about "++show n) $
	  do	tid		<- resolveTypeID tlt n
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
		superTypes	<- superDecls & L.filter (isTid . fst3) |+> canonicalSuperDecl
		let duplicateSupers	= superTypes |> thd3 |> fst & dubbles
		assert (L.null duplicateSupers) $ "Multiple declarations of supertypes. "++show tid++" has multiple instance declarations of "++
			commas (duplicateSupers |> show)
		supers		<- superTypes |+> buildSTTEntry |> M.fromList
		let superOrigins	= supers |> (const fqn)
		return (tid, TypeInfo kind frees constraints' [] supers superOrigins fqn)


buildSTTEntry	:: (RType, [Name], CType) -> Exc (RType, [TypeConstraint])
buildSTTEntry (sub, [], (super, []))
		= return (super, [])
buildSTTEntry (sub, frees, (super, constraints))
		= do	validateFrees frees sub
			let typeConstrs	= constraints |> first RFree & unmerge |> uncurry SubTypeConstr
			return (super, typeConstrs)


canonicalSuperDecl	:: (RType, [Name], CType) -> Exc (RType, [Name], CType)
canonicalSuperDecl (sub, frees, (super, constraints))
		= do	let allFrees	= frees ++ freesInRT sub ++ freesInRT super
			let canonFree	= zip (nub allFrees) defaultFreeNames
			let canonType	= traverseRTM (onFreeM ((|> RFree) . fetch canonFree))
			let frees'	= canonFree |> snd
			sub'	<- canonType sub
			super'	<- canonType super
			constraints'	<- constraints |||>>> canonType ||>> sequence |+> unpackSecond
			constraints'' 	<- constraints' |+> onFirst (fetch canonFree)	:: Exc [(Name, [RType])]
			return (sub', frees', (super', constraints''))

fetch indices free	= L.lookup free indices ? errMsg free
			where errMsg free	= "The free type variable "++free++" was not declared"
