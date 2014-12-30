module Languate.MaintenanceAccess.TestTypeTable2MD where

-- Testing of building and printing of the type table
import StdDef
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Set (Set)
import Data.Map (Map)
import Data.Tuple
import Languate.AST
import Languate.TAST

import Languate.TypeTable
import Languate.TypeTable.TypeTable2MD
import Data.Maybe
import Languate.FQN

import Control.Arrow



-- test statically

tt		= TypeTable (M.map fst knownTypesT) knownTypeReqs superTypesT syns revSyns instConstrT
t 		= putStrLn $ typeTable2MD (genFQN "Prelude") tt

knownTypeReqs	:: Map (RType, Int) (S.Set RType)
knownTypeReqs	= M.unions $ M.elems $ M.mapWithKey buildMap $ M.map snd knownTypesT

buildMap	:: RType -> [(Int, RType)] -> Map (RType, Int) (Set RType)
buildMap nm ls	=  let ireqs = map (\(i,reqs) -> ((nm, i), S.fromList reqs)) $ merge ls in
			M.fromList ireqs

knownTypesT	= M.fromList
			[ ( nat		,(Kind, none))
			, ( natur	,(Kind, none))
			, ( natural	,(Kind, none))
			, ( set		,(KindCurry Kind Kind, [(0,eq)]))
			, ( functor	,(KindCurry Kind Kind, none))
			, ( monoid	,(Kind, none))
			, ( eq		,(Kind, none))]
		where none	= []



natur	= (dataFQN "Nat", "Natural")
natural	= (dataFQN "Nat", "Natur")
set	= genType collectionFQN "Set"
functor	= genType categoryFQN "Functor"
monoid	= genType categoryFQN "Monoid"
eq	= genType categoryFQN "Eq"

superTypesT	= fmap S.fromList $ M.fromList $ map (second rt)
			[ (nat, [eq, monoid])
			, (rt natural, [eq, monoid])
			, (rt set, [functor])
			, (RApplied (rt set) [RFree "a"], [eq])]


rt (fqn,n)	= RNormal fqn n


syns	= M.fromList [( natural, nat),(natur, nat)]
revSyns	= fmap S.fromList $ M.fromList $ merge $ fmap swap $ M.toList syns
instConstrT	= M.fromList [(eq, (eqClassDef, Kind)), (set, (setClassDef, KindCurry Kind Kind))]
eqClassDef	= ClassDef "Eq" [] [] [] "When a type is instance of ''Eq'' it means data of this type can be compared for equivalence." [Example (Just "equiv") (Seq [Call "True", Operator "==", Call "True"]) (Call "True") ] [("==", Curry [Free "eq", Free "eq", Free "eq"], Just "Compare two values", [("eq",Normal [] "Eq")])]

setClassDef	= ClassDef "Set" ["a"] [("a",Normal [] "Eq")] [Normal [] "Collection"] "A set is an unorderd collection" [] [
			("union", Curry [Applied (Normal [] "Collection") [Free "a"], Applied (Normal [] "set") [Free "a"], Applied (Normal [] "set") [Free "a"]], Just "Adds all element to given set", [("a",Normal [] "Eq"),("set", Normal [] "Set")])]

genFQN sub		= toFQN' $ "pietervdvn:Data:" ++ sub
categoryFQN str		= genFQN $ "category." ++ str
collectionFQN str	= genFQN $ "Collection." ++ str
dataFQN str		= genFQN $ "Data." ++ str
numFQN str		= genFQN $ "Num." ++ str

genType fqnGen nm	= (fqnGen nm, nm)
