module Languate.TypeTable.BuildSuperTypeTable where

{--

Builds the super type table, which keeps track of what type is instance of what other type.
Note that these types might be partially applied, **with requirements!**

This table does not verify that types _can be_ subtypes. E.g. instance A is B: buildSuperTypeTable does **not** check wether A implements all the needed functions for B!

e.g.

instance Set (a:Show) is Show

--}

import StdDef

import Exceptions
import Languate.CheckUtils

import Languate.World
import Languate.TypeTable
import Languate.TAST
import Languate.AST
import Languate.FQN
import Languate.TypeTable.Bind.Substitute

import Data.Map hiding (map, null)
import Prelude hiding (lookup)
import Data.Set (Set)
import qualified Data.Set as S
import Data.List (nub)
import Control.Arrow
import Data.Tuple
import Data.Maybe

import Debug.Trace

type Reqs	= [(Name, [RType])]

{-
type SuperTypeTableFor	= Map [Set RType] (Set RType)
type SuperTypeTable	= Map TypeID SuperTypeTableFor
-}

buildSuperTypeTable	:: World -> Map FQN TypeLookupTable -> KindLookupTable
				-> Exc SuperTypeTable
buildSuperTypeTable w tlts klt
		= do	all	<- mapM (try' [] . superTypesIn' tlts klt w) $
					keys $ modules w
			return $ _buildTable $ concat all


superTypesIn'	:: Map FQN TypeLookupTable ->  KindLookupTable -> World -> FQN ->
			Exc [(TypeID, [Name], Reqs, [RType] )]
superTypesIn' tlts klt w fqn
	= do	modul	<- lookup fqn (modules w) ? ("Bug: no module found for "++show fqn)
		tlt	<- lookup fqn tlts ? ("Bug: no tlt found for "++show fqn)
		let inOne stm coor	= onLocation (fqn, coor) $ try' [] $
						superTypeIn (tlt,klt) stm
		mapM (uncurry inOne) (statements' modul) |> concat



-- Returns: (type id which is instance, applied free names, requirements on the frees, supertypes)
superTypeIn	:: (TypeLookupTable, KindLookupTable) ->  Statement ->
			Exc [(TypeID, [Name], Reqs, [RType] )]
superTypeIn tlt (SubDefStm (SubDef nm _ frees supers reqs))
		= superTypeFor tlt [] nm frees supers reqs
superTypeIn tlt (InstanceStm (Instance (path,nm) frees super reqs))
		= superTypeFor tlt path nm frees [super] reqs
superTypeIn tlt (SynDefStm (SynDef nm frees super reqs))
		= superTypeFor tlt [] nm frees [super] reqs
superTypeIn tlt (ClassDefStm cd)
		= superTypeFor tlt [] (name cd) (frees cd) (subclassFrom cd) (classReqs cd)
superTypeIn (tlt, klt) (ADTDefStm (ADTDef nm frees reqs _))
		= do	fqn	<- findTypeOrigin tlt ([], nm)
			reqs'	<- resolveReqs tlt reqs
			let ((tid, frees', reqs''), t)	= normalize klt
				(((fqn, nm), frees, reqs'), anyType)
			return [(tid, frees', reqs'', [t])]
superTypeIn _ _	= return []


{-

Gives a supertype

Args:
- Modulepath	["Collection"]
- Type names	"Set"
- Frees		["a"]
- Supertypes	['Monoid','BooleanAlgebra','Collection a']

Returns:
- A list of: the typeId of Collection.Set, applied frees, requirements on those frees, types it is.

It gets passed through normalizeSTF, which transforms "List a is Collection a" into "List is Collection"

-}
superTypeFor	:: (TypeLookupTable, KindLookupTable) ->
			[Name] -> Name ->
			[Name] -> [Type] -> [TypeRequirement]
			-> Exc [(TypeID, [Name], Reqs, [RType])]
superTypeFor (tlt,klt) path nm frees supers reqs
		= do	fqn	<- findTypeOrigin tlt (path,nm)
			-- no directly known supertypes: add any as supertype
			let tId	= (fqn, nm)
			_supers'	<- resolveTypes tlt supers
			let supers'	= if null _supers' && tId /= anyTypeID then [anyType] else _supers'
			reqs'	<- resolveReqs  tlt reqs
			let unsplit4 ((a, b, c), d)	= (a, b, c, d)

			let sttfs	= unmerge [((tId, frees, reqs'), supers')]
			let normed	= sttfs |> normalize klt
			return $ (merge normed |> unsplit4)


normalize	:: KindLookupTable -> ((TypeID, [Name], Reqs), RType) ->
			((TypeID, [Name], Reqs), RType)
normalize klt entry
	= entry & normalizeSTF & normalizeLength klt

--  Transforms "List a is Collection a" into "List a0 is Collection a0"
normalizeSTF	:: ((TypeID, [Name], Reqs), RType) -> ((TypeID, [Name], Reqs), RType)
normalizeSTF entry@((tid, frees, reqs), super)
	= let	nats	= take (length frees) $ [0..]
		names	= nats |> show |> ('a':)
		binding	= fromList $ zip frees names
		binding'	= Binding (binding |> RFree)
		reqs'	= reqs	|> first (\n -> findWithDefault n n binding)
				|> (||>> substitute binding')	in
		((tid, names, reqs'), substitute binding' super)



-- Adds dummy vars to the frees, so that it's type is fully applied
normalizeLength	:: KindLookupTable -> ((TypeID, [Name], Reqs), RType) ->
			((TypeID, [Name], Reqs), RType)
normalizeLength klt ((tid, frees, reqs), super)
	= let	kind		= findWithDefault Kind tid klt
		needed		= numberOfKindArgs kind
		newFrees	= [1 + length frees .. needed] |> show |> ('b':)
		freesInReqs	= reqs |> snd & concat |> freesInRT & concat
		freesInSuper	= super & freesInRT
		usedFrees	= S.fromList $ (freesInReqs ++ freesInSuper ++ newFrees)
		-- these are the frees that should be renamed
		reboundFrees	= usedFrees S.\\ (S.fromList frees)
		prebinding	= buildBinding $ S.toList reboundFrees
		reqs'		= reqs ||>> (|> substitute prebinding)
		super'		= substitute prebinding super	in
		((tid, frees ++ newFrees, reqs'), super')





resolveReqs	:: TypeLookupTable -> [(Name, Type)] -> Exc Reqs
resolveReqs tlt reqs
		= do	let (names, types)	= unzip reqs
			rtypes	<- resolveTypes tlt types
			return $ merge $ zip names rtypes

-- my cat's contribution to the project: frrrrrrrrrrrrrrrùD£+



-- Builds the primary table
_buildTable	:: [(TypeID, [Name], Reqs, [RType] )] -> SuperTypeTable
_buildTable stuff
		= let 	wrap4 (a,b,c,d)	= (a, (b,c,d))
			stuff'	= merge $ map wrap4 stuff	in
			fromList $ map (second _buildTableFor) stuff'


_buildTableFor	:: [([Name], Reqs, [RType])] -> SuperTypeTableFor
_buildTableFor stuff
		= let	wrap3 (a,b,c) = (a,(b,c))
			stuff'	= merge $ map wrap3 stuff	in
			fromList $ map (second _buildCore) stuff'

_buildCore	:: [ (Reqs, [RType]) ] -> Set (RType, Map Name [RType])
_buildCore  stuff
	 	= let	stuff'	= map _buildElem stuff in
			S.unions stuff'

_buildElem	:: (Reqs, [RType]) -> Set (RType, Map Name [RType])
_buildElem stuff
		= let  	stuff'	= map swap $ unmerge [stuff]	:: [(RType, Reqs)]
			withDict	= fmap (second fromList) stuff' in
			S.fromList withDict
