module Languate.MaintenanceAccess.TestTypeTable2MD where

-- Testing of building and printing of the type table
import StdDef
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Tuple
import Languate.AST
import Languate.TAST

import Languate.TypeTable
import Languate.TypeTable.TypeTable2MD
import Data.Maybe
import Languate.FQN



-- test statically

tt		= TypeTable knownTypesT superTypesT syns revSyns instConstrT
t 		= putStrLn $ typeTable2MD (genFQN "Prelude") tt

knownTypesT	= M.fromList
			[ ( nat		,(Kind, S.empty))
			, ( natur	,(Kind, S.empty))
			, ( natural	,(Kind, S.empty))
			, ( set		,(KindCurry Kind Kind, S.fromList [("a", eq)]))
			, ( functor	,(KindCurry Kind Kind, S.empty))
			, ( monoid	,(Kind, S.empty))
			, ( eq		,(Kind, S.empty))]

natur	= RNormal (dataFQN "Nat") "Natural"
natural	= RNormal (dataFQN "Nat") "Natur"
set	= genType collectionFQN "Set"
functor	= genType categoryFQN "Functor"
monoid	= genType categoryFQN "Monoid"
eq	= genType categoryFQN "Eq"

superTypesT	= fmap S.fromList $ M.fromList
			[ (nat, [eq, monoid])
			, (natural, [eq, monoid])
			, (set, [functor])
			, (RApplied set [RFree "a"], [eq])]


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

genType fqnGen nm	= RNormal (fqnGen nm) nm
