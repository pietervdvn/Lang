module Languate.Typetable.ModuleTraverser (locallyDeclared, declaredType, declaredSuperType) where

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
import Data.Set hiding (map, filter)
import Data.Map hiding (map, filter, mapMaybe)
import Data.Maybe

import Languate.AST as AST
import Languate.TAST
import Languate.FQN
import Languate.Package


-- All locally declared types
locallyDeclared	:: Module -> [(Name, [Name], [TypeRequirement])]
locallyDeclared mod
		=  statements mod |> declaredType & catMaybes



declaredType :: Statement -> Maybe (Name, [Name], [TypeRequirement])
declaredType (ADTDefStm (ADTDef name frees reqs _ _))
		= Just (name, frees, reqs)
declaredType (SubDefStm (SubDef name _ frees _ reqs))
		= Just (name, frees, reqs)
declaredType (ClassDefStm classDef)
		= Just (name classDef, frees classDef, classReqs classDef)
declaredType _	= Nothing



declaredSuperType	:: TypeLookupTable -> Statement -> Exc [(RType, [Name], CType)]
declaredSuperType tlt (ADTDefStm (ADTDef nm frees reqs _ adopts))
	= do	-- the type being declared here is the supertype of the adopted types...
		super	<- resolveTypePath tlt ([], nm)
		-- ... applied on the frees of course!
		let super'	= applyTypeArgs super (frees |> RFree)
		adopts'	<- resolveTypes tlt adopts
		reqs'	<- resolveReqs tlt reqs
		adopts' |> (\typ -> (typ, frees, (super', reqs'))) & return
declaredSuperType tlt (InstanceStm (Instance typePath frees super reqs))
	= do	typ	<- resolveTypePath tlt typePath
		let typ'= applyTypeArgs typ (frees |> RFree)
		super'	<- resolveType tlt super
		reqs'	<- resolveReqs tlt reqs
		(typ', frees, (super', reqs')) & return & return
declaredSuperType tlt (SubDefStm (SubDef name _ frees supers reqs))
	= do	typ	<- resolveTypePath tlt ([], name)
		supers'	<- resolveTypes tlt supers
		reqs'	<- resolveReqs tlt reqs
		supers' |> (\super -> (typ, frees, (super, reqs'))) & return
declaredSuperType tlt (ClassDefStm cd)
	= do	typ	<- ([], AST.name cd) & resolveTypePath tlt
		supers	<- subclassFrom cd & resolveTypes tlt
		reqs	<- classReqs cd & resolveReqs tlt
		let frees	=  AST.frees cd
		supers |> (\super -> (typ, frees, (super, reqs))) & return
declaredSuperType _ _	= return []
