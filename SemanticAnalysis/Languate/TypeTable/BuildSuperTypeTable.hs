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

import Data.Map hiding (map)
import Prelude hiding (lookup)
import Data.Set (Set)
import qualified Data.Set as S
import Control.Arrow
import Data.Tuple


type Reqs	= [(Name, [RType])]

{-
type SuperTypeTableFor	= Map [Set RType] (Set RType)
type SuperTypeTable	= Map TypeID SuperTypeTableFor
-}

buildSuperTypeTable	:: World -> Map FQN TypeLookupTable -> Exc SuperTypeTable
buildSuperTypeTable w tlts
		= do	all	<- mapM (try' [] . superTypesIn' tlts w) $ keys $ modules w
			return $ fixTable $ _buildTable $ concat all


superTypesIn'	:: Map FQN TypeLookupTable -> World -> FQN ->
			Exc [(TypeID, [Name], Reqs, [RType] )]
superTypesIn' tlts w fqn
	= do	modul	<- lookup fqn (modules w) ? ("Bug: no module found for "++show fqn)
		tlt	<- lookup fqn tlts ? ("Bug: no tlt found for "++show fqn)
		let inOne stm coor	= onLocation (fqn, coor) $ try' [] $
						superTypeIn tlt stm
		(mapM (uncurry inOne) $ statements' modul) |> concat



-- Returns: (type id which is instance, applied free names, requirements on the frees, supertypes)
superTypeIn	:: TypeLookupTable ->  Statement -> Exc [(TypeID, [Name], Reqs, [RType] )]
superTypeIn tlt (SubDefStm (SubDef nm _ frees supers reqs))
		= superTypeFor tlt [] nm frees supers reqs
superTypeIn tlt (InstanceStm (Instance (path,nm) frees super reqs))
		= superTypeFor tlt path nm frees [super] reqs
superTypeIn tlt (SynDefStm (SynDef nm frees super reqs))
		= superTypeFor tlt [] nm frees [super] reqs
superTypeIn tlt (ClassDefStm cd)
		= superTypeFor tlt [] (name cd) (frees cd) (subclassFrom cd) (classReqs cd)
superTypeIn _ _	= return []


-- ["Collection"] "Set" ["a"] ['Monoid','BooleanAlgebra']
superTypeFor	:: TypeLookupTable ->
			[Name] -> Name -> [Name] -> [Type] -> [TypeRequirement]
			-> Exc [(TypeID, [Name], Reqs, [RType] )]
superTypeFor tlt path nm frees supers reqs
		= do	fqn	<- findTypeOrigin tlt (path,nm)
			let tId	= (fqn, nm)
			supers'	<- resolveTypes tlt supers
			reqs'	<- resolveReqs  tlt reqs
			return [(tId, frees, reqs', supers')]

resolveReqs	:: TypeLookupTable -> [(Name, Type)] -> Exc Reqs
resolveReqs tlt reqs
		= do	let (names, types)	= unzip reqs
			rtypes	<- resolveTypes tlt types
			return $ merge $ zip names rtypes

-- my cat's contribution to the project: frrrrrrrrrrrrrrrùD£+

-- Adds ''Any'' supertype where needed
fixTable	:: SuperTypeTable -> SuperTypeTable
fixTable	=  mapWithKey fixTableFor


fixTableFor	:: TypeID -> SuperTypeTableFor -> SuperTypeTableFor
fixTableFor tid sttf
		= let	key	= longest $ keys sttf	-- the keys are the 'applied free' names. The longest chain = the defined chain, and should have at least one supertype
			err	= error $ "Huh? No supertype for "++show tid++" "++show key++" while fixing the supertypes?"
			known	= findWithDefault err key sttf
			known'	= if S.null known then S.singleton (anyType, empty) else known		in
			insert key known' sttf



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
