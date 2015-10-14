module Languate.Typetable.PropagateConstraints where

import StdDef
import Exceptions
import Languate.CheckUtils
import HumanUtils


import Graphs.SearchCycles
import Graphs.Order

import Languate.Typetable.TypetableDef
import Languate.Typetable.TypeLookupTable
import Languate.Typetable.ModuleTraverser
import Languate.TAST
import Languate.AST
import Data.Set as S
import Data.Map as M
import Data.List as L
import Data.Maybe
import Data.Either
import Control.Arrow
import Control.Monad

{- Propagates constraints on the super types, e.g. instance List is Bag, the implicit requirement is that (a0:Eq)  -}

{-
Propagates the requirements on the type variables.
e.g.
cat A (a:X)
cat B a:A a

In the second cat, it is implied that B (a:Eq)
-}
propagateParams	:: TypeLookupTable -> Module -> Typetable -> Exc Typetable
propagateParams tlt mod tt@(Typetable dict)
	= do	let depGraph	= dict |> supertypes |> M.keys ||>> getBaseTID |> catMaybes |> S.fromList  :: Map TypeID (Set TypeID)
		order	<- buildOrdering depGraph & either (\e -> halt $ "Cycles in the supertypes: "++show e) (return . reverse . fst)
		adoptions	<- mod & statements |+> constraintAdoptions tlt |> concat |> merge	:: Exc [(RType, [RType])]
		let adoptions'	= adoptions |> (fst &&& id) |> first getBaseTID |> unpackMaybeTuple & catMaybes :: [(TypeID, (RType, [RType]))]
		tt'		<- foldM (\tt@(Typetable content) typeToInv ->
					do	ti	<- M.lookup typeToInv content ? ("No type info found for "++show typeToInv++", weird")
						ti'	<- propagateParamsIn tt adoptions' typeToInv ti
						return $ Typetable $ M.insert typeToInv ti' content ) tt order
		return tt'

propagateParamsIn	:: Typetable -> [(TypeID, (RType, [RType]))] -> TypeID -> TypeInfo -> Exc TypeInfo
propagateParamsIn tt supers tid ti
	= inside ("While propagating the implicit type parameters for the type "++show tid) $
	  case L.lookup tid supers of
		Nothing		-> return ti
		Just (actualForm, supers')
				-> foldM (\ti super -> propagateParamIn tt (actualForm, super) ti) ti supers'

-- fixes a single typeInfo, by adding constraints to the type
propagateParamIn	:: Typetable -> (RType, RType) -> TypeInfo -> Exc TypeInfo
propagateParamIn (Typetable dict) (actualForm, super) ti
	= do	-- what are the type constraints on supertypes from which it 'inherits'
		superTid	<- getBaseTID super ? "A supertype can not be a free"	:: Exc TypeID
		superConstr	<- dict & M.lookup superTid ? ("No typeinfo found for "++show superTid++", weird")
					|> constraints
		warn $ "Adding constraints of "++show super++" to the constraints of "++show actualForm++"\nThese are "++show superConstr
		-- TODO pickup! Actual adding
		return ti
