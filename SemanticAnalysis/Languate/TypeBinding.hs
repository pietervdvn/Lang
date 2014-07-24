module Languate.TypeBinding (bind, bind', fitsIn) where

{--

This module implements typebinding:
> bind (Nat -> b) (Nat -> Int) ((Nat -> b) -> b) = Just ((Nat -> Int) -> Int)

the first two args should be compatible. Out of the first two types, bindings can be derived (b == Int) and substituted in the longer expression

--}

import Languate.AST
import StdDef
import Data.Map (Map, lookup, insert, empty, member)
import qualified Data.Map as Map
import State
import Control.Monad
import Prelude hiding (lookup)
import Data.Maybe
import Normalizable
import Data.Either

-- represents which two types can't be bound together
type NonBinding	= (Type, Type)
type BindTry a	= Either NonBinding a


-- try to bind. If it works, it'll fit
-- expects normalized input
-- TODO work with inheritance table
fitsIn	:: Type -> Type -> Bool
fitsIn (Normal a) (Normal b)
	= a == b
fitsIn _ (Free _)
	= True
fitsIn (Applied t ts) (Applied t' ts')
	= all2 fitsIn (t:ts) (t':ts')
fitsIn (Curry ts) (Curry ts')
	= all2 fitsIn ts ts'
fitsIn (TupleType ts) (TupleType ts')
	= all2 fitsIn ts ts'
fitsIn _ _	= False




-- same as bind, except that it crashes when no binding is possible
bind'	:: Type -> Type -> Type -> Type
bind' a b c
	=  case bind a b c of
		Right typ	-> typ
		Left (a,b)	-> error $ "Could not bind types "++show a++" and "++show b

-- bind	:: Type -> Type -> Type -> Type
-- a is a fragment from inType, which will be substituted with b in the third argument
-- strangeType must be made unique
-- TODO work with inheritance table
bind	:: Type -> Type -> Type -> BindTry Type
bind a b inType
	= do	let (a',tabl)	= makeUnique "a-" a
	 	let (b', tabl')	= makeUnique "b-" b
	 	bindings	<- deriveBindings (normalize a') (normalize b')
		return $ traverse (substitute bindings . subsFrees tabl') inType	-- the subsFree table is needed to get the same substitutions as 'a' got.

substitute	:: [(String,Type)] -> Type -> Type
substitute ((a,t'):binds) (Free a')
	| a == a'	= t'
	| otherwise	= substitute binds $ Free a'
substitute _ t	= t


-- , expects normalized input
deriveBindings	:: Type -> Type -> BindTry [(String, Type)]
deriveBindings (Free a) (Free b)
	| a == b	= return []
	| otherwise	= return [(a, Free b),(b, Free a)]
deriveBindings (Free a) t	= return [(a, t)]
deriveBindings t (Free b)	= return [(b, t)]

deriveBindings (Applied t tps) (Applied t' tps')
	= deriveBindings' (t:tps) (t':tps')
deriveBindings (Curry tps) (Curry tps')
	= deriveBindings' tps tps'
deriveBindings (TupleType tps) (TupleType tps')
	= deriveBindings' tps tps'

deriveBindings a b
	= if a == b then return [] else Left (a,b)


-- free type on the left should be replaced by type on the right
deriveBindings'	:: [Type] -> [Type] -> BindTry [(String, Type)]
deriveBindings' ts ts'	= do	lst	<- mapM (uncurry deriveBindings) $ zip ts ts'
				return $ concat lst

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

-- used in makeuniqe, to substitute 'Free a' stuff into really unique stuff
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
			unless found $ do	index	<- get' next
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

