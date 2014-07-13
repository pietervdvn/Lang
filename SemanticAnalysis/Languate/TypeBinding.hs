module Languate.TypeBinding (bind) where

{--

This module implements typebinding:
> bind (Nat -> b) (Nat -> Int) ((Nat -> b) -> b) = Just ((Nat -> Int) -> Int)

the first two args should be compatible. Out of the first two types, bindings can be derived (b == Int) and substituted in the longer expression

--}

import Languate.AST
import StdDef
import State
import Data.Map (Map, lookup, insert, empty, member)
import qualified Data.Map as Map
import State
import Control.Monad
import Prelude hiding (lookup)
import Data.Maybe
import Normalizable

-- bind	:: Type -> Type -> Type -> Type
-- a is a fragment from inType, strangeType must be made unique
bind	:: Type -> Type -> Type -> Type
bind a b inType
	= let (a',tabl)	= makeUnique "a-" a in
	  let (b', _)	= makeUnique "b-" b in
	  let bindings	= deriveBindings (normalize a') (normalize b') in
		traverse (substitute bindings . subsFrees tabl) inType	-- the subsFree table is needed to get the same substitutions as 'a' got.


substitute	:: [(String,Type)] -> Type -> Type
substitute ((a,t'):binds) (Free a')
	| a == a'	= t'
	| otherwise	= substitute binds $ Free a'
substitute _ t	= t


-- crashes on invalid binding, expects normalized input
deriveBindings	:: Type -> Type -> [(String, Type)]
deriveBindings (Free a) (Free b)
	| a == b	= []
	| otherwise	= [(a, Free b),(b, Free a)]
deriveBindings (Free a) t	= [(a, t)]
deriveBindings t (Free b)	= [(b, t)]

deriveBindings (Applied t tps) (Applied t' tps')
	= deriveBindings' (t:tps) (t':tps')
deriveBindings (Curry tps) (Curry tps')
	= deriveBindings' tps tps'
deriveBindings (TupleType tps) (TupleType tps')
	= deriveBindings' tps tps'

deriveBindings a b
	= if a == b then [] else error $ "Could not bind "++show a ++" and "++show b


-- free type on the left should be replaced by type on the right
deriveBindings'	:: [Type] -> [Type] -> [(String, Type)]
deriveBindings' ts ts'	= concatMap (uncurry deriveBindings) $ zip ts ts'


-- > makeUnique (Curry [Free "a", Free "b", Free "b"]) "c" = Curry [Free "c1", Free "c2", Free "c1"]
-- used before binding to prevent conflicts; e.g. id applied on id:
-- > id id : (a1 -> a1) (b1 -> b1) = ( (b1 -> b1) -> (b1 -> b1) )  (b1 -> b1) = b1 -> b1. 
-- this could go horribly wrong without unique:
-- > id id : (a -> a) (a -> a) = ( (a -> a) -> (a -> a)) (a -> a)
makeUnique	:: String -> Type -> (Type, FreeTable)
makeUnique s t	=  let (_, table)	= runstate (buildTable t) (FreeTable 0 s empty) in
			(traverse (subsFrees table) t, table)



data FreeTable	= FreeTable {next::Int, replaceName::String, conts::Map String Int}
	deriving (Show)


subsFrees	:: FreeTable -> Type -> Type
subsFrees table (Free a)
		= let ind	= lookup a $ conts table in
			case ind of 
				(Just ind)	-> Free $ replaceName table ++ show ind
				Nothing		-> Free a
subsFrees _ t	= t


buildTable	:: Type -> State FreeTable ()
buildTable (Free a)
		=  do	found	<- get' (member a . conts)
			when (not found) $ do	index	<- get' next
						modify $ setNext $ index +1
						cont 	<- get' conts
						modify $ setCont $ insert a index cont
buildTable (Applied t tps)
		=  do	buildTable t
			buildTable' tps
buildTable (Curry tps)	= buildTable' tps
buildTable (TupleType tps)
			= buildTable' tps
buildTable t		= return ()


buildTable'	:: [Type] -> State FreeTable ()
buildTable'	=  mapM_ buildTable

setNext next (FreeTable _ r c)
		= FreeTable next r c

setCont conts (FreeTable n r _)
		= FreeTable n r conts

