module Languate.SymbolTable where

{-- This module implements a symbol table, a map with a fallback parent.
--}
import StdDef
import Prelude hiding (lookup)
import Data.Map hiding (map, null, foldr, foldl)
import qualified Data.Map as Map
import Languate.Signature
import Languate.AST
import Languate.FunctionGenerators
import Languate.FQN
import Data.Maybe
import Data.Either

data SymbolTable a	= Empty
			| Child {parent:: SymbolTable a, content:: Map Signature a}
	deriving (Show)


data TypeTable		= Empt
			| TT {par::TypeTable, cont::Map Name [Type]}

setParent		:: SymbolTable a -> SymbolTable a -> SymbolTable a
setParent p (Child _ cont)
			= Child p cont
setParent p (Empty)	= p

type SimpleTable	= SymbolTable (DocString, [Clause])


find			:: Name -> SymbolTable a -> Maybe a
find _ Empty		=  Nothing
find sign (Child p cont)
			=  let smap	= simpleMap cont in
			   case lookup sign smap of
				Nothing	-> find sign p
				a	-> a


simpleMap		:: Map Signature a -> Map Name a
simpleMap		=  fromList . map (\(Signature name _, a) -> (name, a)) . toList


-- a simple importer without public imports, which gives the symbol tables for each
-- TODO make a decent, cycle proof one
-- TODO make mapping of import -> FQN
buildWithImports	:: Map FQN Module -> Map FQN SimpleTable
buildWithImports mp	=  let simple	= mapWithKey buildLocal mp :: (Map FQN SimpleTable) in
				mapWithKey (addImports simple) mp

addImports		:: Map FQN SimpleTable -> FQN -> Module -> SimpleTable
addImports simple fqn modul
			=  let local	= fromJust $ lookup fqn simple in
			   let imps	= map impToFQN $ rights $ imports modul in
			   let impT	= map (\fqn -> fromJust $ lookup fqn simple) imps in
			   foldl setParent local impT

--TODO
fqpn	= fromJust $ toFQPN "pietervdvn:Data"

impToFQN	:: Import -> FQN
impToFQN (Import _ names name _)
		= fromJust $ toFqn' fqpn names name


-- builds function -> type mapping of locally defined functions.
-- might contain infers
buildLocal	:: FQN -> Module -> SymbolTable (DocString, [Clause])
buildLocal fqn mod
		=  let funcs	= concatMap generate $ statements mod in
			Child Empty $ fromList $ checkDouble' $ map (genSign fqn) $ concatMap undouble funcs


genSign		:: FQN -> Function -> (Signature, (DocString, [Clause]))
genSign fqn (Function doc [(name,typ)] _ imp)
		= (Signature name typ, (doc,imp))

undouble	:: Function -> [Function]
undouble (Function doc nmTypes laws imp)
		= do	(name, typ)	<- nmTypes
			return $ Function doc [(name,typ)] laws imp

checkDouble'	:: [(Signature, a)] -> [(Signature, a)]
checkDouble' ls	=  let (signs, as)	= unzip ls in
			zip (checkDouble signs) as

checkDouble	:: [Signature] -> [Signature]
checkDouble signs
		= if null $ dubbles signs then signs
			else error $ "Double signatures exist: "++ show (map show $ dubbles signs)
