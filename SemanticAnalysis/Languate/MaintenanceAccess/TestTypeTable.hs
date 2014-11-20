module TestTypeTable where

-- Testing of building and printing of the type table
import StdDef
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Tuple
import Languate.AST
import Languate.TAST

import Languate.TypeTable.TypeTable
import Languate.TypeTable.TypeTable2MD
import Data.Maybe
import Languate.FQN


tt		= TypeTable knownTypesT superTypesT syns revSyns instConstrT
t 		= putStrLn $ typeTable2MD fqn tt

knownTypesT	= S.fromList [(Normal "Nat", Kind "*", S.empty)
			, (Normal "Natural", Kind "*", S.empty)
			, (Normal "Natur", Kind "*", S.empty)
			, (Normal "Set", KindCurry [Kind "a"] $ Kind "*", S.fromList [("a", Normal "Eq")])
			, (Normal "Functor", KindCurry [Kind "*"] $ Kind "*", S.empty)
			, (Normal "Monoid", Kind "*", S.empty)
			, (Normal "Eq", Kind "*", S.empty)]

superTypesT	= fmap S.fromList $ M.fromList [(Normal "Nat", [Normal "Eq", Normal "Monoid"]), (Normal "Natural", [Normal "Eq", Normal "Monoid"]), (Normal "Set", [Normal "Functor"]),( Applied (Normal "Set") [Normal "a"], [Normal "Eq"])]

syns	= M.fromList [(Normal "Natural", Normal "Nat"),(Normal "Natur", Normal "Nat")]
revSyns	= fmap S.fromList $ M.fromList $ merge $ fmap swap $ M.toList syns
instConstrT	= M.fromList [(Normal "Eq", (eqClassDef, Kind "*")), (Normal "Set", (setClassDef, KindCurry [Kind "a"] $ Kind "*"))]
eqClassDef	= ClassDef "Eq" [] [] [] "When a type is instance of ''Eq'' it means data of this type can be compared for equivalence." [Example (Just "equiv") (Seq [Call "True", Operator "==", Call "True"]) (Call "True") ] [("==", Curry [Normal "eq", Normal "eq", Normal "eq"], Just "Compare two values", [("eq",Normal "Eq")])]

setClassDef	= ClassDef "Set" ["a"] [("a",Normal "Eq")] [Normal "Collection"] "A set is an unorderd collection" [] [
			("union", Curry [Applied (Normal "Collection") [Free "a"], Applied (Normal "set") [Free "a"], Applied (Normal "set") [Free "a"]], Just "Adds all element to given set", [("a",Normal "Eq"),("set", Normal "Set")])]

fqn		= fromJust $ toFQN "pietervdvn:Data:Data.Prelude"
