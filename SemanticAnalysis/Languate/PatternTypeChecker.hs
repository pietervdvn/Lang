module Languate.PatternTypeChecker where

{--

This module (type)checks patterns, and produces a typed closure as byproduct.

Checks and crashes on
-> Double assignments
-> Invalid deconstructions
-> Mulitdontcare gets unpacked

--}

import StdDef
import State
import Normalizable

import Data.Map (member, insert, empty, Map)

import Languate.AST
import Languate.TAST
import Languate.SymbolTable
import Languate.TypeTable
import Languate.TypeChecker
import Languate.Signature
import Languate.Precedence.Precedence

import Control.Monad
import Control.Monad.Reader
import Data.Maybe

import Debug.Trace


type TypedClosure	= Map Name Type

checkPattern	:: (TypeTable, PrecedenceTable) -> [Pattern] -> [Type] -> ([TPattern], Map Name Type)
checkPattern tt patterns types
		= let patterns'	= map normalize $ sanitize (length types) patterns in
			runstate (checkAll tt patterns' types) empty

-- expands multi-dontcare into the proper amount of dont-cares
sanitize	:: Int -> [Pattern] -> [Pattern]
sanitize 0 []	=  []
sanitize _ []	=  []
sanitize i (MultiDontCare:pts)
		= replicate (i - length pts) DontCare ++ pts
sanitize i (pt:pts)
		= pt:sanitize (i-1) pts


checkAll	:: (TypeTable, PrecedenceTable) -> [Pattern] -> [Type] -> State TypedClosure [TPattern]
checkAll ctx pts	tps
	| length pts == length tps
		=  zipWithM (checkOne ctx) pts tps
	| otherwise
		=  error $ "Too much/too little patterns given in comparison with the given types " ++ show pts

checkOne	:: (TypeTable,PrecedenceTable) -> Pattern -> Type -> State TypedClosure TPattern
checkOne _ (Assign name) t
		= do	st	<- get
			when (name `member` st) $ error $ "Duplicate name detected in pattern: "++name
			modify (insert name t)
			return $ TAssign name
checkOne _ (Let _ _) _
		= todos "Let expressions in patterns are not supported yet"
checkOne ctx (Deconstruct function patterns) t
		= do	let funcType	= findType function $ fst ctx	-- the types that the given function might have
			let err		= error $ "Deconstruction function not found (for pattern): "++function
			let typs	= fromMaybe err funcType
			let deconstrType	= filter (validFunctionType (length patterns) t) $ map normalize typs	-- now filtered for a -> Mabye (.,.,.)
			let err'	= error $ "The deconstructor function "++function++" does not have the right type, expected function of type "++show (validType t)++", but we only found types "++show typs++"\n"++show ctx
			let deconstrType' = if (null deconstrType) then err' else deconstrType
			-- there can only be one function with this signature
			let [actualFunctionType]	= deconstrType'
			let producedTypes		= returnedType actualFunctionType
			-- producedTypes are now the types that the subpatterns should match
			-- sanitized to remove embedded multidontcares
			let patterns'	= sanitize (length producedTypes) patterns	-- TODO * will be considered a _ now
			patterns	<- checkAll ctx patterns' producedTypes
			-- the signature is the signature of the function that is called. The type will thus typically be ''a -> (b,c,d,...)?''
			return $ TDeconstruct (Signature function actualFunctionType) patterns


checkOne _ (DontCare) _
		= return TDontCare
checkOne ctx (Multi patterns) t
		= do	patterns'	<- mapM (flip (checkOne ctx) t) patterns
			return $ TMulti patterns'
checkOne ctx (Eval e) t
		= do	let prefExpr	= expr2prefExpr (snd ctx) e
			let texp	= runReader (typeCheck e) $ fst ctx
			unless (t `elem` typeOf texp) $ error $ "Patterntypcheck: eval: the expression in the eval-pattern does not have the right type: it is of type "++show (typeOf texp)++" instead of the expected type "++show t
			return $ TEval texp
checkOne _ MultiDontCare _
		= error "Patterns: not sanitized. This is a bug"

validType	:: Type -> Type
validType t	=  Curry [t, Applied (Normal "Maybe") [Free "(.,.,..)"]]



-- converts a valid deconstructor-functiontype into the types it returns (thus the tuple types)
returnedType		:: Type -> [Type]
returnedType (Curry [_, Applied (Normal "Maybe") [TupleType ts]])
			= ts
returnedType (Curry [_, Applied (Normal "Maybe") [t]])
			= [t]
returnedType _	= error "Bug: Semantal/PatternTypeChecker/returnedType: no sanitize before returned type; should not happen"

-- Curry [Applied (Normal "Wrap") [Free "a"],Applied (Normal "Maybe") [TupleType [Free "a"]]]]),
-- returns true if the type is of the form ''a -> (b,c,d,...)?'' with exactly i tuple arguments
validFunctionType	:: Int -> Type -> Type -> Bool
validFunctionType i expSource (Curry [actSource, Applied (Normal "Maybe") [TupleType ts]])
	=  i == length ts && normalize expSource == normalize actSource
validFunctionType i expSource (Curry [actSource, Applied (Normal "Maybe") [_]])
	=  i == 1 && normalize expSource == normalize actSource
validFunctionType _ _ t	=  False
