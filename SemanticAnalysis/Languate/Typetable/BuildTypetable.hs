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
		typeInfos'	<- locDecl |+> buildTypeInfo tlt superDecls fqn |> M.fromList
		let tt		= Typetable typeInfos'
		-- now propagate existence constraints
		tt'		<- propagateImplicitConstraints tlt stms tt |> fst
		constrComplete	<- addSuperConstraints tlt stms tt'
		-- then we calculate the 'transitive closure' of the supertype relationship
		-- for non mathematicians: X is a Y; Y is a Z => X is a Z
		superComplete	<- propagateSupertypes constrComplete
		-- and as last, we pass over all type info, to check constraints
		checkTT superComplete
		return superComplete


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
