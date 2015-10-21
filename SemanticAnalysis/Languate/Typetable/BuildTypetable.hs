module Languate.Typetable.BuildTypetable where


import StdDef
import HumanUtils
import Exceptions
import Languate.CheckUtils

import Languate.AST hiding (frees)
import Languate.TAST
import Languate.FQN

import Languate.Typetable.TypetableDef
import Languate.Typetable.TypeLookupTable
import Languate.Typetable.ModuleTraverser
import Languate.Typetable.PropagateConstraints

import Languate.Checks.CheckType

import Data.Set as S
import Data.Map as M
import Data.List as L
import Data.Maybe
import Graphs.SearchCycles

import Control.Arrow hiding ((+++))


{- builds the 'defined' type table from the code.
	-> Direct constraints on the free types are loaded

	The known table is needed for the kinds kinds
-}
buildTypetable	:: Module -> TypeLookupTable -> FQN -> Exc Typetable
buildTypetable mod tlt fqn
		= inside ("While building the type info in module "++show fqn) $
		  do	let locDecl	= locallyDeclared mod 	:: [(Name, [Name], [TypeRequirement])]
			superDecls	<- mod & statements |> declaredSuperType tlt & sequence |> concat
						:: Exc [(RType, [Name], CType)]
			typeInfos	<- locDecl |+> buildTypeInfo tlt superDecls fqn |> M.fromList
			checkSupertypeCycles (typeInfos |> supertypes)
			constrainedTT	<- propagateConstraints tlt mod (Typetable typeInfos) |> fst
			return constrainedTT


propagateConstraints tlt mod tt
	= (`whileChanged` tt)
		(\tt_ -> do	-- first we propagate the implicit type requirements on cat-declarations
				-- as long as we are changing: propagete the implicit type parameters; this might not be complete
				(tt', changed')		<- whileChanged (propagateParams tlt mod) tt_
				-- now the implicit requirements on requirement: X a (b:Set a) => a:Eq
				(tt'', changed'')	<- whileChanged propagateMetaRequirements tt'
				return (tt'', changed' || changed''))


whileChanged	:: Monad m => (a -> m (a, Bool)) -> a -> m (a, Bool)
whileChanged act a
		= do	res <- whileDo' snd (\((a_, onceChanged), _)	->
				do	(a', changed)	<- act a_
					return ((a', changed || onceChanged), changed)) ((a, False), False)
			return $ fst res



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
		let isTid t	= getBaseTID t |> ((==) tid) & fromMaybe False	:: Bool
		superTypes	<- superDecls & L.filter (isTid . fst3) |+> canonicalSuperDecl
		let duplicateSupers	= superTypes |> thd3 |> fst & dubbles
		assert (L.null duplicateSupers) $ "Multiple declarations of supertypes. "++show tid++" has multiple instance declarations of "++
			commas (duplicateSupers |> show)
		supers		<- superTypes |+> buildSTTEntry |> M.fromList
		return (tid, TypeInfo kind frees constraints' [] supers)


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



checkSupertypeCycles  :: Map TypeID (Map RType a) -> Check
checkSupertypeCycles supers
		= do	--  we build a graph of supertypes, and check for cycles
			let graph	= supers |> M.keys ||>> getBaseTID  |> catMaybes |> S.fromList
			let cycles	= cleanCycles graph
			cycles |+> (\cycle -> err $ "Types form a cycle in the supertypes: "++ (cycle |> show & commas))
			return ()
