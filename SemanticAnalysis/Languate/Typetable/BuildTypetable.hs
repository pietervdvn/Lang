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

import Languate.Checks.CheckType
import Control.Monad

import Data.Set as S
import Data.Map as M
import Data.List as L
import Data.Maybe

import Graphs.DirectedGraph
import Graphs.ExportCalculator (calculateExports, calculateImports)

import Control.Arrow hiding ((+++))

type SupertypeRel	= (RType, [Name], CType)

{- Let' s build all the type tables.
	We attempt to keep calculating the TTs as local as possible
		(thus only relying on the own tt)
	but this is not always possible.
  Return (defined, known, exposed) for each fqn
	-}
buildTypetables	:: Package -> Map FQN TypeLookupTable -> Exc (Map FQN Typetable, Map FQN Typetable, Map FQN Typetable)
buildTypetables p tlts
	= do	-- first, build the 'defined'-table for all modules
		let buildDefTT (fqn, mod)
			= do	tlt	<- M.lookup fqn tlts ? ("No tlt for the module "++show fqn++" found, this is a bug")
				defined	<- buildTypetable tlt fqn mod
				return (fqn, defined)
		defineds	<- p & modules & M.toList |+> buildDefTT |> M.fromList	:: Exc (Map FQN (Typetable, [SupertypeRel]))
		-- defineds' only containts he locally declared values. This means that the TI's will always be the same if we propagate them
		-- we'll add supertype declarations out other modules later on
		let defineds'	= defineds |> fst |> (\(Typetable tt) -> tt)	:: Map FQN (Map TypeID TypeInfo)
		-- we build the collection of typ






		-- now, we export the basic type tables along the import graph
		let ig		= p & importGraph
		let eg		= invertDict ig
		let fetch fqn	= M.findWithDefault S.empty fqn defineds'	:: Set (TypeID, TypeInfo)
		-- filter function which asks if 'source' may re-export the given tid, defined in origin
		let filter source (origin, (tid, ti))
				= True	-- TODO filter types
		let exports	= calculateExports ig eg fetch filter	:: Map FQN (Set ((TypeID, TypeInfo), FQN))
		let knowns	= calculateImports ig fetch exports	:: Map FQN (Set ((TypeID, TypeInfo), FQN))
		-- as only locally declared types are propagated for now, we know that all TI's will be the same and no merging will be needed

		-- helper function which  flattens the properties graph...
		let prep props	= props |> S.toList
					||>> (\((tid, ti),fqn) -> (tid, (ti, fqn)))
					|> merge 	:: Map FQN [(TypeID, [(TypeInfo, FQN)])]
		-- ...and actually merge the type infos of props
		let mergeTIs' fqn vals
				= vals ||>> merge |+> mergeTIs fqn	:: Exc [(TypeID, TypeInfo)]
		let mergeTTs props
				= dictMapM mergeTIs' (prep props)
					||>> M.fromList	||>> Typetable
		exports'	<- mergeTTs exports	:: Exc (Map FQN Typetable)
		knowns'		<- mergeTTs knowns	:: Exc (Map FQN Typetable)
		return (defineds |> fst, exports', knowns')



mergeTIs	:: FQN -> (TypeID, [(TypeInfo, [FQN])]) -> Exc (TypeID, TypeInfo)
mergeTIs fqn (tid, [(ti, _)])
		=  return (tid, ti)
mergeTIs fqn (tid, tisFQNs)
		= do	return (tid, fst $ head tisFQNs)

{- builds the 'defined' type table from the code.
	-> Direct constraints on the free types are loaded

	The known table is needed for the kinds kinds
-}
buildTypetable	:: TypeLookupTable -> FQN -> Module -> Exc (Typetable, [SupertypeRel])
buildTypetable tlt fqn mod
	= inside ("While building the local type info in module "++show fqn) $
	  do	-- first build the locally known values
		let locDecl	= locallyDeclared mod 	:: [(Name, [Name], [TypeRequirement])]
		-- now we get all defined supertypes (inclusing synonyms)
		directSupers	<- mod & statements |> declaredSuperType tlt & sequence |> concat
					:: Exc [(RType, [Name], CType)]
		synons	<- mod & statements |+> typeSynonyms tlt |> concat
				||>> (\(rt, frees, super) -> (rt, frees, (super,[])))	:: Exc [(RType, [Name], CType)]
		let superDecls	= directSupers ++ synons
		{- we now build the supertype table for locally declared values.
			It picks out of 'superDecls' the supertypes it needs;
			this means that supertypes which are declared about a foreign type, are ignored.
		-}
		typeInfos'	<- locDecl |+> buildTypeInfo tlt superDecls fqn |> M.fromList
		-- these are the supertype declarations that were ignored
		let missed	= superDecls & L.filter (\vals -> (vals & fst3 & getBaseTID |> (`M.member` typeInfos') |> not) & fromMaybe True)
		return (Typetable typeInfos', missed)


completifyTypetable	:: Module -> TypeLookupTable -> FQN -> Typetable -> Exc Typetable
completifyTypetable mod tlt fqn tt
	= inside ("While completing the type table of "++show fqn) $
	  do	-- now propagate existence constraints
		tt'		<- propagateImplicitConstraints tlt mod tt |> fst
		constrComplete	<- addSuperConstraints tlt mod tt'
		-- then we calculate the 'transitive closure' of the supertype relationship
		-- for non mathematicians: X is a Y; Y is a Z => X is a Z
		superComplete	<- propagateSupertypes constrComplete
		-- and as last, we pass over all type info, to check constraints (and remove them)
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
buildTypeInfo	:: TypeLookupTable -> [(RType, [Name], CType)] -> FQN -> (Name, [Name], [(Name, Type)]) -> Exc (TypeID, TypeInfo)
buildTypeInfo tlt superDecls fqn (n, frees, reqs)
	= inside ("While building type info about "++show fqn++"."++n) $
	  do	let tid		= (fqn, n)
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
		return (tid, TypeInfo kind frees constraints' [] supers fqn)


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
