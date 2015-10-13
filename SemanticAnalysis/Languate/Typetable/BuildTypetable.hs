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

import Languate.Checks.CheckType

import Data.Map as M
import Data.List as L
import Data.Maybe

import Control.Arrow hiding ((+++))


{- builds the 'defined' type table from the code.
	-> Direct constraints on the free types are loaded
-}
buildTypetable	:: Module -> TypeLookupTable -> FQN -> Exc Typetable
buildTypetable mod tlt fqn
		= do	let locDecl	= locallyDeclared mod 	:: [(Name, [Name], [TypeRequirement])]
			superDecls	<- mod & statements |> declaredSuperType tlt & sequence |> concat
						:: Exc [(RType, [Name], CType)]
			locDecl |> buildTypeInfo tlt superDecls fqn & sequence |> M.fromList |> Typetable


-- Builds the type info about the given type in the tuple
buildTypeInfo	:: TypeLookupTable -> [(RType, [Name], CType)] -> FQN -> (Name, [Name], [(Name, Type)]) -> Exc (TypeID, TypeInfo)
buildTypeInfo tlt superDecls fqn (n, frees, reqs)
	= inside ("While building type info about "++show fqn++"."++n) $
	  do	let tid		= (fqn, n)
		-- about the free type names
		let indices	= zip frees [0..]	:: [(Name, Int)]
		reqs'		<- reqs |+> (\(nm, t) -> do rt<-resolveType tlt t;return (nm, rt) )	:: Exc [(Name, RType)]
		reqs' |> snd |+> validateFrees frees
		-- about the constraints

		constraints	<- reqs' |+>  onFirst (fetch indices)  :: Exc [(Int, RType)]
		let constraints'= constraints & merge & M.fromList
		-- about the supertypes$
		let isTid t	= getBaseTID t |> ((==) tid) & fromMaybe False	:: Bool
		superTypes	<- superDecls & L.filter (isTid . fst3) |+> canonicalSuperDecl
		let duplicateSupers	= superTypes |> thd3 |> fst & dubbles
		assert (L.null duplicateSupers) $ "Multiple declarations of supertypes. "++show tid++" has multiple instance declarations of "++
			commas (duplicateSupers |> show)
		supers		<- superTypes |+> buildSTTEntry |> M.fromList
		return (tid, TypeInfo frees constraints' supers)


buildSTTEntry	:: (RType, [Name], CType) -> Exc (RType, [TypeConstraint])
buildSTTEntry (sub, [], (super, []))
		= return (super, [])
buildSTTEntry (sub, frees, (super, constraints))
		= do	validateFrees frees sub
			let typeConstrs	= constraints |> first RFree & unmerge |> uncurry SubTypeConstr
			return (super, typeConstrs)


canonicalSuperDecl	:: (RType, [Name], CType) -> Exc (RType, [Name], CType)
canonicalSuperDecl (sub, frees, (super, constraints))
		= do	let canonFree	= zip frees defaultFreeNames
			let canonType	= traverseRTM (onFreeM ((|> RFree) . fetch canonFree))
			let frees'	= canonFree |> snd
			sub'	<- canonType sub
			super'	<- canonType super
			constraints'	<- constraints |||>>> canonType ||>> sequence |+> unpackSecond
			constraints'' 	<- constraints' |+> onFirst (fetch canonFree)	:: Exc [(Name, [RType])]
			return (sub', frees', (super', constraints''))


fetch indices free	= L.lookup free indices ? errMsg free
			where errMsg free	= "The free type variable "++free++" was not declared"
