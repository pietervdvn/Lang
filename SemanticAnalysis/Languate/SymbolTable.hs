module Languate.SymbolTable where

{-- This module implements a symbol table and type table, a map with a fallback parent and related functions.
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

instance Functor SymbolTable where
	fmap f Empty	= Empty
	fmap f (Child p cont)
			= Child (fmap f p) (Map.map f cont)

mapWithType		:: (Type -> a -> b) -> SymbolTable a -> SymbolTable b
mapWithType _ Empty	=  Empty
mapWithType f (Child p cont)
			= Child (mapWithType f p) $ Map.mapWithKey (\(Signature _ t) a -> f t a) cont

filterTable		:: (a -> Bool) -> SymbolTable a -> SymbolTable a
filterTable _ Empty	=  Empty
filterTable f (Child p cont)
			= Child (filterTable f p) $ Map.filter f cont

stFromList		:: [(Signature,a)] -> SymbolTable a
stFromList		=  Child Empty . fromList 


setParent		:: SymbolTable a -> SymbolTable a -> SymbolTable a
setParent p (Child _ cont)
			= Child p cont
setParent p (Empty)	= p

find			:: Name -> SymbolTable a -> Maybe [a]
find _ Empty		=  Nothing
find sign (Child p cont)
			=  let smap	= simpleMap cont in
			   case lookup sign smap of
				Nothing	-> find sign p
				a	-> a

signatures		:: SymbolTable a -> [Signature]
signatures Empty	=  []
signatures (Child p cont)
			= signatures p ++ keys cont


-- there might be multiple entries for the same name (but with different types); thats why a list in the return
simpleMap	:: Map Signature a -> Map Name [a]
simpleMap	=  fromList . merge . map (\(Signature name _, a) -> (name, a)) . toList

unzipST		:: SymbolTable (a,b)	-> (SymbolTable a, SymbolTable b)
unzipST Empty	=  (Empty, Empty)
unzipST (Child p cont)
		=  let (a,b)	= unzipST p in
		   let (a',b')	= (Map.map fst cont, Map.map snd cont) in
			(Child a a', Child b b')

lookupSt		:: Signature -> SymbolTable a -> Maybe a
lookupSt _ Empty	= Nothing
lookupSt sign (Child p cont)
			= case lookup sign cont of
				Nothing		-> lookupSt sign p
				(Just a)	-> Just a
