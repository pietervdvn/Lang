module Languate.Interpreter.Utils where

{--

Some misc functions, all used in Interpreter
--}

import StdDef
import Languate.FQN
import Languate.AST
import Languate.Signature
import Languate.SymbolTable
import Languate.InterpreterDef
import Languate.TypedPackage

import Normalizable

import Control.Monad.Reader
import Data.Maybe (fromMaybe)
import qualified Data.Map as M

data Context	= Context {world::TypedPackage}
	deriving (Show)
type RC	a	= Reader Context a


getModule	:: FQN -> RC TModule
getModule fqn	=  do	tpack	<- fmap world $ ask
			let err	= error $ "Interpreter error: location not known: "++show fqn
			return $ fromMaybe err $ M.lookup fqn tpack


returnedType	:: Type -> Type
returnedType (Curry (_:t))
		= normalize $ Curry t
returnedType t	= error $ "Not a curry that can return values! "++show t


wrapAsApp	:: Value -> [Value] -> Value
wrapAsApp thunk args
		=  let t	= typeOfValue thunk in
			App (map returnedType t) thunk args


-- true if the value represents nothing
isNothingV	:: Value -> Bool
isNothingV (ADT (Applied (Normal "Maybe") _) 0 _)
		= True
isNothingV _	= False


validDeconstructor	:: Type -> Int -> Type -> Bool
validDeconstructor t i (Curry [source, Applied (Normal "Maybe") [(TupleType xs)]])
			= (i == length xs) && (t == source)
validDeconstructor _ _ _	= False		

-- Searches the deconstructor function for given name, type and number of arguments. Multiple signatures are possible. The fqn is the context in which the functions are searched.
_searchDF	:: FQN -> Name -> Type -> Int -> RC [Signature]
_searchDF fqn name typ nrOfArgs
		= do	mod	<- getModule fqn
			let signs	=  signaturesWithName name $ functions mod
			return $ filter (\(Signature _ actType) -> validDeconstructor typ nrOfArgs actType) signs
	
